-module(docker_handler).

-export([handle/2]).

handle(Path, File) ->
    {ok, Regex} = re:compile("^deployments/.+.json$"),
    case re:run(File, Regex) of
        {match, _Captured} -> handle_deployment(Path, File);
        nomatch -> ok
    end.

handle_deployment(Path, DeploymentFile) ->
    logger:info("found deployment file: ~s", [DeploymentFile]),
    {ok, Json} = file:read_file(Path ++ "/" ++ DeploymentFile),
    Deployment = jsone:decode(Json),
    ContainerList = lists:map(fun binary:bin_to_list/1,
                              maps:get(<<"containers">>, Deployment)),
    Containers = sets:from_list(ContainerList),
    {ok, PsOutput} = imp_docker:ps(["-q", "--no-trunc"]),
    CurrentContainers = sets:from_list(string:split(PsOutput, "\n", all)),
    case sets:size(sets:subtract(Containers, CurrentContainers)) of
        0 -> noop;
        _ ->
            {ok, Container} = deployment_handler:deploy(Deployment),
            write_container_update(Container, Deployment, Path, DeploymentFile),
            update
    end.

write_container_update(Container, Deployment, Path, File) ->
    BinContainers = [binary:list_to_bin(Container)],
    NewDeployment = Deployment#{<<"containers">> => BinContainers},
    Json = jsone:encode(NewDeployment, [{indent, 2}, {space, 1}]),
    ok = file:write_file(Path ++ "/" ++ File, Json).

