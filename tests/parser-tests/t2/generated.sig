LEXEMS:
    1 : 1  - PROGRAM (256)
    2 : 5  - ABC321 (32769)
    2 : 11 - ; (129)
    3 : 1  - PROCEDURE (259)
    3 : 11 - P000 (32770)
    3 : 16 - ; (129)
    4 : 1  - PROCEDURE (259)
    4 : 11 - P000 (32770)
    4 : 17 - ( (131)
    4 : 17 - ) (132)
    4 : 18 - ; (129)
    5 : 1  - PROCEDURE (259)
    5 : 11 - PPPP (32771)
    5 : 27 - ; (129)
    6 : 1  - PROCEDURE (259)
    6 : 11 - ADD (32772)
    6 : 15 - ( (131)
    6 : 15 - A (32773)
    6 : 16 - : (128)
    6 : 17 - FLOAT (263)
    6 : 22 - ; (129)
    6 : 24 - B (32774)
    6 : 25 - : (128)
    6 : 26 - FLOAT (263)
    6 : 30 - SIGNAL (260)
    6 : 36 - ; (129)
    6 : 37 - ) (132)
    6 : 38 - ; (129)
    7 : 1  - BEGIN (257)
    7 : 7  - END (258)
    7 : 10 - ; (129)
TREE:

<PROGRAM>
..<IDENTIFIER>
....[32769:ABC321]
..<BLOCK>
....<STATEMENTS-LIST>
......<NIL>
....<DECLARATIONS>
......<PROCEDURE>
........<IDENTIFIER>
..........[32770:P000]
........<PARAMETERS-LIST>
......<PROCEDURE>
........<IDENTIFIER>
..........[32770:P000]
........<PARAMETERS-LIST>
......<PROCEDURE>
........<IDENTIFIER>
..........[32771:PPPP]
........<PARAMETERS-LIST>
......<PROCEDURE>
........<IDENTIFIER>
..........[32772:ADD]
........<PARAMETERS-LIST>
..........<DECLARATION>
............<IDENTIFIERS-LIST>
..............<IDENTIFIER>
................[32773:A]
............<ATTRIBUTES-LIST>
..............[263:FLOAT]
..........<DECLARATION>
............<IDENTIFIERS-LIST>
..............<IDENTIFIER>
................[32774:B]
............<ATTRIBUTES-LIST>
..............[263:FLOAT]
..............[260:SIGNAL]
IDENTIFIERS:
    B       -- 32774
    A       -- 32773
    ADD     -- 32772
    PPPP    -- 32771
    P000    -- 32770
    ABC321  -- 32769
