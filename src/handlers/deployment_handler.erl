-module(deployment_handler).

-export([handle/2, deploy/1]).

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
    {ok, Container} = update_version(Deployment),
    write_deployment_details(ExpVsn, Container, Deployment, Path,
                             DeploymentFile),
    update.

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
    deploy(Deployment).


update_deployment(ExpVsn, ActVsn, Deployment) ->
    Id = binary:bin_to_list(maps:get(<<"id">>, Deployment)),
    logger:info("migrating ~s from version ~s to version ~s",
                [Id, ExpVsn, ActVsn]),
    % Need to remove old containers first as they own ports
    OldContainers = lists:map(fun binary:bin_to_list/1,
                              maps:get(<<"containers">>, Deployment)),
    case imp_docker:stop(OldContainers) of
        ok ->
            ok = imp_docker:rm(OldContainers);
        {error, Err} ->
            logger:error("failed to stop container: ~p", [Err])
    end,
    deploy(Deployment).

% TODO move this elsewhere
deploy(Deployment) ->
    Vsn = binary:bin_to_list(maps:get(<<"expected_version">>, Deployment)),
    Id = binary:bin_to_list(maps:get(<<"id">>, Deployment)),
    Cmd = lists:map(fun binary_to_list/1, maps:get(<<"cmd">>, Deployment, [])),
    docker_deploy(Id, Vsn, build_options(Deployment), Cmd).

write_deployment_details(Vsn, Container, Deployment, Path, File) ->
    BinContainers = [binary:list_to_bin(Container)],
    NewDeployment = Deployment#{<<"actual_version">> => Vsn,
                                <<"containers">> => BinContainers},
    Json = jsone:encode(NewDeployment, [{indent, 2}, {space, 1}]),
    ok = file:write_file(Path ++ "/" ++ File, Json).


docker_deploy(Id, Vsn, Opts, Cmd) ->
    case imp_docker:run(Id, Vsn, Opts, Cmd) of
        Ok={ok, Container} ->
            % TODO healthcheck
            logger:info("deploy of ~s:~s successful with ID ~s~n",
                        [Id, Vsn, Container]),
            Ok;
        Err={error, Error} ->
            logger:error("failed to deploy ~s:~s: ~s", [Id, Vsn, Error]),
            Err
    end.

build_options(Deployment) ->
    Args0 = build_env_args(maps:get(<<"environment">>, Deployment, #{})),
    Args1 = build_publish_args(maps:get(<<"publish">>, Deployment, [])),
    Args2 = build_volume_args(maps:get(<<"volumes">>, Deployment, [])),
    Args0 ++ Args1 ++ Args2.

build_env_args(EnvMap) ->
    List = maps:to_list(EnvMap),
    F = fun({Key, Val}) ->
                ["-e", binary:bin_to_list(<<Key/binary, "=", Val/binary>>)]
        end,
    lists:flatmap(F, List).

build_publish_args(PublishList) ->
    F = fun(#{<<"external">> := Ext, <<"internal">> := Int}) ->
                ["-p", to_list(Ext) ++ ":" ++ to_list(Int)]
        end,
    lists:flatmap(F, PublishList).

build_volume_args(VolumeList) ->
    F = fun(V=#{<<"host_path">> := Host, <<"container_path">> := Container}) ->
                Options = enlist(maps:get(<<"options">>, V, [])),
                Parts = lists:map(fun to_list/1, [Host, Container|Options]),
                ["-v", string:join(Parts, ":")]
        end,
    lists:flatmap(F, VolumeList).

to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_integer(V) -> integer_to_list(V).

enlist(V) when is_list(V) -> V;
enlist(V) -> [V].
