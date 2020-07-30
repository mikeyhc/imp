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
    if ExpVsn =/= ActVsn -> update_version(Deployment);
       true ->
           logger:info("no changes for deployment ~s",
                       [maps:get(<<"id">>, Deployment)])
    end.

update_version(Deployment) ->
    ExpVsn = maps:get(<<"expected_version">>, Deployment),
    ActVsn = maps:get(<<"actual_version">>, Deployment),
    if ActVsn =:= null ->
           first_deployment(ExpVsn, Deployment);
       true ->
           update_deployment(ExpVsn, ActVsn, Deployment)
    end.

first_deployment(Vsn, Deployment) ->
    logger:info("deploying ~s for the first time, using version ~s",
                [maps:get(<<"id">>, Deployment), Vsn]).

update_deployment(ExpVsn, ActVsn, Deployment) ->
    logger:info("migrating ~s from version ~s to version ~s",
                [maps:get(<<"id">>, Deployment), ExpVsn, ActVsn]).
