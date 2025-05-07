SELECT COL(0), 
                MERGE(COL(1), COL(5)), 
                MERGE(COL(2), COL(6)), 
                MERGE(COL(3), COL(7))
    FROM A 
    INNER JOIN B ON A.COL(0) == B.COL(0),
    ORDER BY ASC;
