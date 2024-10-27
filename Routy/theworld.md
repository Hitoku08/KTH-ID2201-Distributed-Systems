erl -name america@192.168.31.189 -setcookie routy -connect_all false

{nice, 'france@192.168.31.148'} ! {add,california,{california,'america@192.168.31.189'}}.

{beijing, 'china@192.168.31.55'} ! {add,california,{california,'america@192.168.31.189'}}.