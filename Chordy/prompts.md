## node1
Pid1=node1:start(1).
Pid2=node1:start(2,Pid1).
Pid3=node1:start(3,Pid1).
Pid4=node1:start(4,Pid1).
Pid5=node1:start(5,Pid1).
Pid10=node1:start(10,Pid1).



## node2
Pid1=node2:start(1).
Pid5=node2:start(5,Pid1).
Pid10=node2:start(10,Pid1).
Pid50=node2:start(50,Pid1).
Pid100=node2:start(100,Pid1).

test:add(2,love,Pid1).
test:add(7,like,Pid1).
test:add(10,work,Pid1).
test:add(11,world,Pid1).
test:lookup(7,Pid1).

Pid8=node2:start(8,Pid1).

## node3
Pid1=node3:start(1).
Pid2=node3:start(2,Pid1).
Pid3=node3:start(3,Pid1).
Pid4=node3:start(4,Pid1).

Pid2 ! stop.

## test
Pid=test:start(node2). 
test:start(node2,10,Pid). 
test:add(10,iloveyou,Pid).
test:lookup(10,Pid).

