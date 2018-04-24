module SegmentModule

type Coordinate = (int * int) // x, y coordinate of a pixel
type Colour = byte list       // one entry for each colour band, typically: [red, green and blue]

type Segment = 
    | Pixel of Coordinate * Colour
    | Parent of Segment * Segment 

let mapreduce m r = (List.map m) >> (List.reduce r)

let sqr x : float  = x * x

let inline add x y = x + y

let inline mul (x:float) (y:float) : float = y * x

let inline sub x y = x - y

let sum  = List.reduce (+)

let rec createColourList segment  =
    match segment with
    |Pixel(_, col) -> [yield col]
    |Parent(seg1,seg2) -> [yield! (createColourList seg1); yield! (createColourList seg2)]

let createRedColourBand (segment: Segment) (i : int) : float list =
    let col = segment |> createColourList 
    [yield float(col.Item(i).Item(0))]

let createGreenColourBand (segment: Segment) (j : int) : float list =
    let col = segment |> createColourList 
    [yield float(col.Item(j).Item(1))]
   
let createBlueColourBand (segment: Segment) (k : int) : float list=
    let col = segment |> createColourList 
    [yield float(col.Item(k).Item(2))]

// - function which calculates standard deviation of a list of numbers
let standardDeviation (colourBand : float list) : float = 
    let length = mapreduce (fun x -> 1) (+)
    let mean = (sum colourBand) / float(length colourBand)
    let variance = colourBand |> List.averageBy (fun x -> (sqr(x - mean)))
    sqrt(variance)

// return a list of the standard deviations of the pixel colours in the given segment
// the list contains one entry for each colour band, typically: [red, green and blue]
let stddev (segment: Segment) : float list =
    //raise (System.NotImplementedException())

    let stdevR = createRedColourBand segment 0 |> standardDeviation
    let stdevG = createGreenColourBand segment 1|> standardDeviation
    let stdevB = createBlueColourBand segment 2|> standardDeviation

    [stdevR;stdevG;stdevB]

// determine the cost of merging the given segments: 
// equal to the standard deviation of the combined the segments minus the sum of the standard deviations of the individual segments, 
// weighted by their respective sizes and summed over all colour bands
let mergeCost segment1 segment2 : float = 
    //raise (System.NotImplementedException())
    let pixel = segment1 |> createColourList
    let pixel_length = pixel.Length
    let pixel_lengthFloat = float(pixel_length)
    let col1 = stddev segment1 
    let col1Red = col1.Item(0)
    let col1Green = col1.Item(1)
    let col1Blue = col1.Item(2)

    let pixel2 = segment2 |> createColourList
    let pixel_length2 = pixel2.Length
    let pixel_lengthFloat2 = float(pixel_length2)
    let col2 = stddev segment2 
    let col2Red = col2.Item(0)
    let col2Green = col2.Item(1)
    let col2Blue = col2.Item(2)

    let pixlencom = add pixel_lengthFloat pixel_lengthFloat2
    let comcol = stddev (Parent(segment1,segment2))
    let comcolRed = comcol.Item(0)
    let comcolGreen = comcol.Item(1)
    let comcolBlue = comcol.Item(2)

    let merge_costR = (sub (mul comcolRed pixlencom) (add (mul col1Red pixel_lengthFloat) (mul col2Red pixel_lengthFloat2)))
    let merge_costG = (sub (mul comcolGreen pixlencom) (add (mul col1Green pixel_lengthFloat) (mul col2Green pixel_lengthFloat2)))
    let merge_costB = (sub (mul comcolBlue pixlencom) (add (mul col1Blue pixel_lengthFloat) (mul col2Blue pixel_lengthFloat2)))

    let merge_costh = add merge_costR merge_costG

    add merge_costh merge_costB