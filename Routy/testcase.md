routy:start(r1, stockholm).
routy:start(r2, uppsala).
routy:start(r3, linkoping).
routy:start(r4, gothenburg).
routy:start(r5, lund).

r1 ! {add, uppsala, r2}.
r2 ! {add, stockholm, r1}.
r1 ! {add, linkoping, r3}.
r3 ! {add, stockholm, r1}.
r3 ! {add, gothenburg, r4}.
r4 ! {add, linkoping, r3}.
r2 ! {add, lund, r5}.
r5 ! {add, uppsala, r2}.
r4 ! {add, lund, r5}.
r5 ! {add, gothenburg, r4}.

r1 ! broadcast.
r2 ! broadcast.
r3 ! broadcast.
r4 ! broadcast.
r5 ! broadcast.

r1 ! update.
r2 ! update.
r3 ! update.
r4 ! update.
r5 ! update.

r1 ! {send, lund, 'Hi, I love you!'}.

r1 ! broadcast.
r3 ! broadcast.
r4 ! broadcast.
r5 ! broadcast.

r1 ! update.
r3 ! update.
r4 ! update.
r5 ! update.