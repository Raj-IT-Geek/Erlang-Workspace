-module(ping).
-export([reachable/1, serviceable/2]).

reachable(Host) ->
  os:cmd("ping -c 1 "++Host++" &>/dev/null; echo $?") == "0\n".

serviceable(Host, Service) ->
  os:cmd("ssh "++Host++ " \"systemctl |grep running|grep "
         ++Service++"|wc -l\"") == "1\n".
