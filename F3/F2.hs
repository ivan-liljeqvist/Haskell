
--Ivan Liljeqvist and Filip Martinsson

module F2 where
    import Data.List

    class Evol a where
        distance::a->a->Double
        name::a->String
        distanceMatrix::[a]->[(String,String,Double)]
        distanceMatrix [] = []
        distanceMatrix a = (map (\x -> (name (head a), name x, distance (head a) x)) (a))++distanceMatrix (tail a)


    data MolSeq = MolSeq {molName::String,theSeq::String,isDNA::Bool} deriving (Show) 
    data Profile = Profile {profName::String,
                          numberOfSeq::Int,
                          isProfDNA::Bool,
                          matrix::[[(Char,Double)]]} 
                          deriving (Show)

    instance Evol MolSeq where
        distance a b = seqDistance a b
        name molseq = seqName molseq
        

    instance Evol Profile where
        distance a b = profileDistance a b
        name profile = profileName profile
        


    {-
        matrixDistanceMolSeq takes a list of MolSeq and returns a matrix of absolute difference with all possible combinations

        With map we go through one molseq at a time and check distance between that one and every other in the list.
        (head molseqs) - the one we compare to everyother
        Then we append the result of the recursion when we only give it the tail
        and then (head molseqs) will be the second element in the array and it will be compared to every elements that are left in the list.

        At the end we have a matrix with the absolute distances with 
    

    matrixDistanceMolSeq :: [MolSeq] -> [(String,String,Double)]
    matrixDistanceMolSeq [] = []
    matrixDistanceMolSeq molseqs = (map (\x -> (name (head molseqs), name x, seqDistance (head molseqs) x)) (molseqs))++matrixDistanceMolSeq (tail molseqs)

    
        matrixDistanceProfile takes a list of MolSeq and returns a matrix of absolute difference with all possible combinations.
        The same operations are performed as in matrixDistanceMolSeq
    

    matrixDistanceProfile :: [Profile] -> [(String, String, Double)]
    matrixDistanceProfile [] = []
    matrixDistanceProfile profiles = (map (\x -> (name (head profiles), name x, profileDistance (head profiles) x)) (profiles))++matrixDistanceProfile (tail profiles)
    -}
    
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
    seqName = molName  

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
        Returns: Double
    -}
    seqDistance :: MolSeq -> MolSeq -> Double
    seqDistance seq1 seq2 = 
        if isSameType seq1 seq2 then
            --If its DNA
            if (isDNA seq1) then
                if(getHammingDistance seq1 seq2) > 0.74 then 
                    3.3
                else 
                    abs(getDNADistance (getHammingDistance seq1 seq2))

            --If its Protein
            else
                if(getHammingDistance seq1 seq2) > 0.94 then 
                   3.7
                else 
                    abs(getProteinDistance (getHammingDistance seq1 seq2))
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
        isArraySameType
        Takes an array of  sequences.
        Returns true if all are of the same type.
        False if they aren't
    -}

    isArraySameType :: [MolSeq] -> Bool
    isArraySameType (seqOne:seqTwo:rest) = if(isSameType seqOne seqTwo) then isArraySameType (seqTwo:rest)
                                            else False
    isArraySameType theLast = True


    {-|
        getHammingDistance
        Takes two sequences.
        Returns Hamming distance, the number of differences between the sequences.
    -}

    getHammingDistance :: MolSeq -> MolSeq -> Double
    --Make tuples of each postition, filter where they are different. Divide by the original length
    getHammingDistance x y = fromIntegral (length(filter (\a -> fst a /= snd a) (zip (theSeq x) (theSeq y)))) / fromIntegral(length (theSeq y))

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

    {-|
        profileName
        Takes in a Profile and returns the name of it
    -}

    profileName :: Profile -> String
    profileName profile = profName profile

    {-|
        profileDistance
        Takes in two profiles and returns the distance between them using d(M,M′)
    -}

    profileDistance :: Profile -> Profile -> Double
    {-|
        1) Take the matrices and turn them into plain 1D arrays using concat.
        2) Zip the 2 matrices. So we have big tuples that contain both tuble at a particular coordinate.
           For example: ((A,1),(A,3)) tuple with tuples
        3) Now we go through each pair and get the difference between the values (second place in the tuple)
        4) We do so by using map and mapping out anon function on each tuple. Each tuple will give us a Double.
        5) We take the absolute value when we subtract each paor. 
        6) Now we have an array of all the differences [Double]
        7) We now sum the array so we get a Double.
    -}
    profileDistance p1 p2 = sum arrayWithAbsoluteDifferences 
        where 
        plainMatArray1 = concat (matrix p1)
        plainMatArray2 = concat (matrix p2)
        tupleWithTuplesAtEachCoord = (zip plainMatArray1 plainMatArray2) --array of tuples with tuples at every coordinate
        subtractPairAndAbs = (\(x,y) -> abs(snd x - snd y)) --function used by map below
        arrayWithAbsoluteDifferences = (map subtractPairAndAbs tupleWithTuplesAtEachCoord)



    {-|
        profileFrequency
        Takes in Profile, position, character and returns the relative frequence of the character
        at the position in the Profile.
    -}


    profileFrequency :: Profile -> Int -> Char -> Double
    --1) take the matrix as mat
    --2) go to the row we're interested at with mat!!i
    --3) now we filter this row to only have tuples starting with letter 'c'
    --4) The idea is that filter should return only 1 element, because each row has only unique letters
    --5) We extract this element with !!0
    --6) now we have extracted the tuble we're after. The value we want is in the second position
    --7) Therefore use snd to extract it
    profileFrequency profile i c = snd((filter((==c).fst)(mat!!i))!!0) where mat = matrix profile



    {-|
        molseqs2profile
        Takes the name of profile to be created, several MolSeqs and constructs a Profile. 
    -}

    molseqs2profile :: String -> [MolSeq] -> Profile
    molseqs2profile pName seqs = 
        if not (isArraySameType seqs) then
            error "not same type, cant make profile"
        --construct the profile. because we made sure that they all are same type, we just take the
        --type from the first element
        else Profile pName (length seqs) (isDNA (seqs!!0))  relMatrix
            where relMatrix=(makeRelativeMatrix (makeProfileMatrix seqs) (length seqs))


    {-|
        makeRelativeMatrix
        Takes the matrix with absolute values and makes them realtive.
    -}

    makeRelativeMatrix :: [[(Char, Int)]] -> Int -> [[(Char, Double)]]
    --1) We need to go through each tuple in every row and divide the Double value by the number of sequences
    --2) Go through each row in the matrix.
    --3) Go through each tuple in that row.
    --4) For each tuple divide the second value (y) by the number of sequences.
    --5) Now we have a relative matrix.
    makeRelativeMatrix absMatrix numberOfSeqs = 
        map (map (\(x, y) -> (x , fromIntegral y / fromIntegral (numberOfSeqs)))) absMatrix



    {-|
        makeProfileMatrix
        Construct a profile matrix
    -}

    nucleotides = "ACGT"
    aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

    makeProfileMatrix :: [MolSeq] -> [[(Char,Int)]]
    makeProfileMatrix [] = error "Empty sequence list"
    makeProfileMatrix sl = 
        if not (isArraySameType sl) then
            error "not same type, cant make matrix"
        else res where 
                currentSeq = head sl

                --create default tubles for each letter. we'll get [(A,0),[C,0]...]
                defaults = 
                  --do it first for DNA
                  if (isDNA currentSeq) then
                    zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)
                  --repeat for Protein
                  else 
                    zip aminoacids (replicate (length aminoacids) 0)   -- Rad (ii)
                --extract the sequences from each Seq Object and return an array of strings
                strs = map seqSequence sl -- Rad (iii)

                --1) Transpose the matrix
                --2) Sort each row in the matrix (sort)
                --3) Divide each row into arrays containing the same letters (group)
                --4) We now have something like this: [["AA","CCC"],["A","GGGG"]..]
                --5) Return a 2D array of tuples. Each tuble is made by taking the 
                --first element in each "letter array element ex: 'AA'" and the length of the element.
                --so in the case of 'AA' we'll get the tuple (A,2)
                --6) At the end of this function we get tmp which is a 2D array of such tuples.
                --Ex: [[(A,2),(C,3)],[(G,1),(T,1)]] 
                -- How we did it step by step: https://www.dropbox.com/s/2w28arudqzmcr9c/progp2.jpg?dl=0
                -- https://www.dropbox.com/s/t1ileqhbps2t4b1/progp1.jpg?dl=0
                
                tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
                           (transpose strs)      
                                                 -- Rad (iv)
                --check for each element in the sublist of tmp1
                --unionBy will use this and fill the matrix with 'default tuples' which letters are missing
                equalFst a b = (fst a) == (fst b) 

                --use unionBy with equalFst to fill the matrix with default tuples. Sort the matrix
                res = map sort (map (\l -> unionBy equalFst l defaults) tmp1) -- 
 





        





