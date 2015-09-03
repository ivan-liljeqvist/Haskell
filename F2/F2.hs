
--Ivan Liljeqvist

module F2 where

    data MolSeq = MolSeq String String Bool deriving (Show)  

    {-|
      string2seq - takes the name and the sequence and parses it into MolSeq
      param 1: the name of the sequence
      param 2: the sequence
      returns MolSeq with the sequence parsed
      returns 'untitled sequence' in MolSeq as name if no name supplied
      returns 'no sequence' in MolSeq as sequence if no sequence supplied
    -}

    string2seq :: String -> String -> MolSeq
    string2seq (a:xs) (y:ys) = string2seqHelper (x:xs) (y:ys) (y:ys)
    string2seq _ (y:ys) = MolSeq "untitled sequence" (y:ys) True
    string2seq (x:xs) _ = MolSeq (x:xs) "no sequence" True
    string2seq _ _ = MolSeq "untitled sequence" "no sequence" True

    
    {-|
      string2seqHelper - stores the original sequence.
      param 1: the name of the sequence
      param 2: the remaining of the sequence we haven't checked yet
      param 3: the original sequence
      returns MolSeq with the sequence parsed
    -}

    string2seqHelper :: String -> String -> String -> MolSeq
    string2seqHelper (x:xs) (y:ys) (z:zs) = 

        --if still DNA, continue running recursively
        if(y `elem` "agctAGCT")then
            string2seqHelper (x:xs) (ys) (z:zs)
        else --if Protein, return
            MolSeq (x:xs) (z:zs) False

    --funtion run when we've run through the entire sequence and haven't find any trace of Protein
    string2seqHelper (x:xs) _ (z:zs) = MolSeq (x:xs) (z:zs) True


        





