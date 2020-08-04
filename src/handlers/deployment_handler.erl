-module(deployment_handler).

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
    ExpVsn = maps:get(<<"expected_version">>, Deployment),
    ActVsn = maps:get(<<"actual_version">>, Deployment),
    if ExpVsn =/= ActVsn ->
           {ok, Container} = update_version(Deployment),
           write_deployment_details(ExpVsn, Container, Deployment, Path,
                                    DeploymentFile),
           update;
       true ->
           logger:info("no changes for deployment ~s",
                       [maps:get(<<"id">>, Deployment)]),
           noop
    end.

update_version(Deployment) ->
    ExpVsn = maps:get(<<"expected_version">>, Deployment),
    ActVsn = maps:get(<<"actual_version">>, Deployment),
    if ActVsn =:= null ->
           first_deployment(ExpVsn, Deployment);
       true ->
           update_deployment(ExpVsn, ActVsn, Deployment)
    end.

first_deployment(BinVsn, Deployment) ->
    Vsn = binary:bin_to_list(BinVsn),
    Id = binary:bin_to_list(maps:get(<<"id">>, Deployment)),
    logger:info("deploying ~s for the first time, using version ~s", [Id, Vsn]),
    case imp_docker:run(Id, Vsn) of
        Ok={ok, Container} ->
            % TODO healthcheck
            logger:info("deploy of ~s:~s successful with ID ~s~n",
                        [Id, Vsn, Container]),
            Ok;
        Err={error, Error} ->
            logger:error("failed to deploy ~s:~s: ~s", [Id, Vsn, Error]),
            Err
    end.


update_deployment(ExpVsn, ActVsn, Deployment) ->
    Vsn = binary:bin_to_list(ExpVsn),
    Id = binary:bin_to_list(maps:get(<<"id">>, Deployment)),
    logger:info("migrating ~s from version ~s to version ~s",
                [Id, ExpVsn, ActVsn]),
    case imp_docker:run(Id, Vsn) of
        Ok={ok, Container} ->
            % TODO healthcheck new container
            logger:info("deploy of ~s:~s successful with ID ~s~n",
                        [Id, Vsn, Container]),
            OldContainers = lists:map(fun binary:bin_to_list/1,
                                      maps:get(<<"containers">>, Deployment)),
            case imp_docker:stop(OldContainers) of
                ok ->
                    ok = imp_docker:rm(OldContainers);
                {error, Err} ->
                    logger:error("failed to stop container: ~p", [Err])
            end,
            Ok;
        Err={error, Error} ->
            logger:error("failed to deploy ~s:~s: ~s", [Id, Vsn, Error]),
            Err
    end.

write_deployment_details(Vsn, Container, Deployment, Path, File) ->
    BinContainers = [binary:list_to_bin(Container)],
    NewDeployment = Deployment#{<<"actual_version">> => Vsn,
                                <<"containers">> => BinContainers},
    Json = jsone:encode(NewDeployment, [{indent, 2}, {space, 1}]),
    ok = file:write_file(Path ++ "/" ++ File, Json).
