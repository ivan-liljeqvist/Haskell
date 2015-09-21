
--Ivan Liljeqvist and Filip Martinsson

module Main where
    import F2
    import Data.List

    main :: IO ()
    main =  
        do  
        inputStr <- getContents 
        putStrLn ("11")

    {-
        nJ - The algorithm that takes in several MolSeqs and returns the tree
    -}

    nJ::[MolSeq]->String
    nJ molseqs = 

    {-
        parseRows - Takes in a string and returns an array of string splitted by \n

        1) split the string by \n
        2) we'll get array of strings like this ["aaa","\nbbb","\nccc"]
        3) now we need to remove the \n from all strings except the first one
        4) we take the tail (allButFirstInSplitted) and remove the first character on each string
        5) we then take head (firstStringInSplitted) and append it as first element
    -}

    parseRows::String -> [String]
    parseRows inStr = firstStringInSplitted:map(\x -> tail x) allButFirstInSplitted 
        where 
            splittedString = (groupBy (\a b -> b /= '\n') inStr)
            firstStringInSplitted = head splittedString
            allButFirstInSplitted = tail splittedString


    {-
        row2MolSeqDNA - Takes in a string which has the format "name sequence" (a row form stdin)
        and creates a MolSeq with the name, sequence and isDNA set to true
    -}

    row2MolSeqDNA::String -> MolSeq
    row2MolSeqDNA rowFromInput = string2seq n s 
        where 
            n = head (words rowFromInput) --the first element after we split is the name
            s = last (words rowFromInput) --the second element after we split is the sequence





        





