SELECT P.COL(0), 
                MERGE(P.COL(1), Q.COL(1)), 
                MERGE(P.COL(2), Q.COL(2)), 
                MERGE(P.COL(3), Q.COL(3))
    FROM P 
    INNER JOIN Q ON P.COL(0) == Q.COL(0)
    ORDER BY ASC;
