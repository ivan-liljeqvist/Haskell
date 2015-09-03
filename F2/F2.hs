
--Ivan Liljeqvist and Filip Martinsson

module F2 where
    import Data.List

    data MolSeq = MolSeq {name::String,theSeq::String,isDNA::Bool} deriving (Show) 

    a = MolSeq "a" "ACGTACGT" True 
    b = MolSeq "a" "CCCTACCT" True 


    {-|
      string2seq - takes the name and the sequence and parses it into MolSeq
      param 1: the name of the sequence
      param 2: the sequence
      returns MolSeq with the sequence parsed
      returns 'untitled sequence' in MolSeq as name if no name supplied
      returns 'no sequence' in MolSeq as sequence if no sequence supplied
    -}

    string2seq :: String -> String -> MolSeq
    string2seq (x:xs) (y:ys) = string2seqHelper (x:xs) (y:ys) (y:ys)
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

    {-|
        seqName    
        Returns the name of the sequence data structure.
        Param1: the MolSeq sequence
        Returns: name of the MolSeq in Param1
    -}

    seqName :: MolSeq -> String 
    seqName seq = name seq 

    {-|
        seqSequence
        Returns the sequence of the sequence data structure.
        Param1: the MolSeq sequence
        Returns: sequence of the MolSeq in Param1
    -}
    
    seqSequence :: MolSeq -> String 
    seqSequence seq = theSeq seq 

    {-|
        seqLength
        Returns the length of the sequence of the sequence data structure.
        Param1: the MolSeq sequence
        Returns: length of the sequence of the MolSeq in Param1
    -}
    
    seqLength :: MolSeq -> Int 
    seqLength seq = length (seqSequence (seq))

    {-|
        seqDistance
        Returns the evolutionary distance between two sequences.
        Throws error if trying to compare protein with DNA.
        Param1: sequence 1
        Param1: sequence 2
        Returns: ??
    -}
    seqDistance :: MolSeq -> MolSeq -> Double
    seqDistance seq1 seq2 = 
        if isSameType seq1 seq2 then
            --If its DNA
            if (isDNA seq1) then
                if(getHammingDistance seq1 seq2) > 0.74 then 
                    3.3
                else 
                    getDNADistance (getHammingDistance seq1 seq2)

            --If its Protein
            else
                if(getHammingDistance seq1 seq2) <= 0.94 then 
                    3.7
                else 
                    getProteinDistance (getHammingDistance seq1 seq2)
        else
            error("Sequences are not of the same type")

    {-|
        isSameType
        Takes two sequences.
        Returns true if both are of the same type.
        False if they aren't
    -}

    isSameType :: MolSeq -> MolSeq -> Bool
    isSameType seq1 seq2 = if ((isDNA seq1 && isDNA seq2) || (not (isDNA seq1) && not (isDNA seq2))) then True else False

    {-|
        getHammingDistance
        Takes two sequences.
        Returns Hamming distance, the number of differences between the sequences.
    -}

    getHammingDistance :: MolSeq -> MolSeq -> Double
    --Make tuples of each postition, filter where they are different. Divide by the original length
    getHammingDistance x y = fromIntegral (length(filter (\a -> fst a /= snd a) (zip (theSeq x) (theSeq y)))) / fromIntegral(length (theSeq x))

    {-|
        getDNADistance
        Takes Hamming distance. Returns evolutionary distance for DNA.
    -}

    getDNADistance :: Double -> Double
    getDNADistance a = -(3/4) * log (1-4*a/3)

    {-|
        getProteinDistance
        Takes Hamming distance. Returns evolutionary distance for Protein.
    -}

    getProteinDistance :: Double -> Double
    getProteinDistance a = -(19/20) * log (1-20*a/19)




        





