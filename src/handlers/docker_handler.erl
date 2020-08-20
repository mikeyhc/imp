-module(docker_handler).

-export([handle/2]).

-record(port, {local_port :: integer(),
               host_ip :: binary(),
               host_port :: integer(),
               protocol :: string()
              }).

-record(mount, {source :: binary(),
                destination :: binary(),
                read_write :: boolean()
               }).

-record(container, {id :: binary(),
                    image :: binary(),
                    command :: [binary()],
                    environment :: maps:map(string(), string()),
                    ports :: [#port{}],
                    mounts :: [#mount{}],
                    labels :: maps:map(binary(), binary())
                   }).

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
    CurrentContainers = sets:from_list(lists:map(fun(X) -> X#container.id end,
                                                 get_containers())),
    case sets:size(sets:subtract(Containers, CurrentContainers)) of
        0 -> noop;
        _ ->
            {ok, Container} = deployment_handler:deploy(Deployment),
            write_container_update(Container, Deployment, Path, DeploymentFile),
            update
    end.

get_containers() ->
    {ok, PsOutput} = imp_docker:ps(["-q", "--no-trunc"]),
    CurrentContainers = string:split(PsOutput, "\n", all),
    lists:map(fun(X) -> read_container(X) end, CurrentContainers).

write_container_update(Container, Deployment, Path, File) ->
    BinContainers = [binary:list_to_bin(Container)],
    NewDeployment = Deployment#{<<"containers">> => BinContainers},
    Json = jsone:encode(NewDeployment, [{indent, 2}, {space, 1}]),
    ok = file:write_file(Path ++ "/" ++ File, Json).


read_container(Id) ->
    {ok, Json} = imp_docker:inspect(Id),
    [Container] = jsone:decode(list_to_binary(Json)),
    Config = maps:get(<<"Config">>, Container),
    EnvFn = fun(E, Acc) ->
                    [Key, Value] = binary:split(E, <<"=">>),
                    Acc#{Key => Value}
            end,
    Env = lists:foldl(EnvFn, #{}, maps:get(<<"Env">>, Config)),
    PortFn = fun({_, null}) -> false;
                ({K, [V]}) ->
                     [LocalPort, Proto] = binary:split(K, <<"/">>),
                     {true, #port{local_port = binary_to_integer(LocalPort),
                                  host_ip = maps:get(<<"HostIp">>, V),
                                  host_port = binary_to_integer(
                                                maps:get(<<"HostPort">>, V)),
                                  protocol = Proto
                                 }}
             end,
    Network = maps:get(<<"NetworkSettings">>, Container),
    Ports = lists:filtermap(PortFn,
                            maps:to_list(maps:get(<<"Ports">>, Network))),
    MountFn = fun(M) ->
                      #mount{source = maps:get(<<"Source">>, M),
                             destination = maps:get(<<"Destination">>, M),
                             read_write = maps:get(<<"RW">>, M)
                            }
              end,
    Mounts = lists:map(MountFn, maps:get(<<"Mounts">>, Container)),
    #container{id = Id,
               image = maps:get(<<"Image">>, Config),
               command = maps:get(<<"Cmd">>, Config),
               environment = Env,
               ports = Ports,
               mounts = Mounts,
               labels = maps:get(<<"Labels">>, Config)
              }.
