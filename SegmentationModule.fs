module SegmentationModule

open SegmentModule

// Maps segments to their immediate parent segment that they are contained within (if any) 
type Segmentation = Map<Segment, Segment>

// Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
let rec findRoot (segmentation: Segmentation) segment : Segment =
    let map1 = segmentation
    let result = Map.tryFind segment map1
    match result with
    |Some segment -> (findRoot segmentation segment)
    |None -> segment

// Initially, every pixel/coordinate in the image is a separate Segment
// Note: this is a higher order function which given an image, 
// returns a function which maps each coordinate to its corresponding (initial) Segment (of kind Pixel)
let createPixelMap (image:TiffModule.Image) : (Coordinate -> Segment) =
    let pixelMap (coord:Coordinate) =
        let colourlist = TiffModule.getColourBands image coord
        Pixel(coord, colourlist)
    pixelMap


// Find the neighbouring segments of the given segment (assuming we are only segmenting the top corner of the image of size 2^N x 2^N)
// Note: this is a higher order function which given a pixelMap function and a size N, 
// returns a function which given a current segmentation, returns the set of Segments which are neighbours of a given segment
let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> Set<Segment>) =
    let rec neighbourPixels (segment: Segment) : seq<Coordinate> = 
        let width = 1 <<< N
        let height = 1 <<< N
        match segment with
        | Pixel ((x,y), _) ->
                seq { 
                    if (x+1 < width) then
                        yield (x+1,y)
                    if (y+1 < height) then
                        yield (x,y+1) 
                    if (x > 0) then
                         yield (x-1,y)
                    if (y > 0) then
                         yield (x,y-1) }
        | Parent (seg1, seg2) -> seq{yield! (neighbourPixels seg1); yield! (neighbourPixels seg2)}

    let neighbourPix (segment: Segment) : seq<Segment> =
        let coordinates = neighbourPixels segment
        coordinates |> Seq.map (fun x -> pixelMap x)

    let neighbhours (segmentation:Segmentation) (segment: Segment) : Set<Segment> =
        //raise (System.NotImplementedException())
        let segmentseq = neighbourPix segment
        let neighbourpixels = Seq.map (findRoot segmentation) segmentseq |> Set.ofSeq |> Set.remove segment
        neighbourpixels
    neighbhours

// The following are also higher order functions, which given some inputs, return a function which ...


 // Find the neighbour(s) of the given segment that has the (equal) best merge cost
 // (exclude neighbours if their merge cost is greater than the threshold)
let createBestNeighbourFunction (neighbours:Segmentation->Segment->Set<Segment>) (threshold:float) : (Segmentation->Segment->Set<Segment>) =
    let bestNeighbours (segmentation: Segmentation) (segment:  Segment) : Set<Segment> = 
        let neighboursAll = neighbours segmentation segment 
        let setGuard = 
            if (neighboursAll.IsEmpty) then 
                Set.empty
            else
                neighboursAll
        let mergecosts = Set.map (mergeCost segment) setGuard
        let bestCostNeightbour seg = 
            let cost = mergeCost segment seg
            let bestMergeCost = mergecosts.MinimumElement
            cost <= threshold && cost <= bestMergeCost && bestMergeCost <= threshold
        Set.filter bestCostNeightbour neighboursAll
    bestNeighbours


// Try to find a neighbouring segmentB such that:
//     1) segmentB is one of the best neighbours of segment A, and 
//     2) segmentA is one of the best neighbours of segment B
// if such a mutally optimal neighbour exists then merge them,
// otherwise, choose one of segmentA's best neighbours (if any) and try to grow it instead (gradient descent)
let createTryGrowOneSegmentFunction (bestNeighbours:Segmentation->Segment->Set<Segment>) (pixelMap:Coordinate->Segment) : (Segmentation->Coordinate->Segmentation) =
    let rec tryGrowOneSegmentHelp (segmentation : Segmentation) (segment: Segment) : Segmentation =
        let bestNeighboursofSegment = bestNeighbours segmentation segment
        let isEachotherBestNeighbours segment2 =
           let bestNeighbourofSegment2 = bestNeighbours segmentation segment2
           Set.contains segment bestNeighbourofSegment2
        let bestNeighbourSegments = Set.filter isEachotherBestNeighbours bestNeighboursofSegment
        if (bestNeighbourSegments.IsEmpty) then
            if (bestNeighboursofSegment.IsEmpty) then
                segmentation
            else
                let bestNeoghboursofSegmentAlist = Set.toList bestNeighboursofSegment
                let segment2 = List.head bestNeoghboursofSegmentAlist
                tryGrowOneSegmentHelp segmentation segment2
        else 
            let bestNeighboursSegmentslist = Set.toList bestNeighbourSegments
            let segment2 = List.head bestNeighboursSegmentslist
            let ParentNew = Parent(segment, segment2)
            segmentation
            |> Map.add segment ParentNew
            |> Map.add segment2 ParentNew

    let tryGrowOneSegment (segmentation: Segmentation) (coord: Coordinate) : Segmentation =
        let baseSegment = pixelMap coord
        let rootSegement = findRoot segmentation baseSegment
        tryGrowOneSegmentHelp segmentation rootSegement
    tryGrowOneSegment

// Try to grow the segments corresponding to every pixel on the image in turn 
// (considering pixel coordinates in special dither order)
let createTryGrowAllCoordinatesFunction (tryGrowPixel:Segmentation->Coordinate->Segmentation) (N:int) : (Segmentation->Segmentation) =
    let tryGrowAllCoordinates(segmentation : Segmentation) : Segmentation =
        Seq.fold tryGrowPixel segmentation (DitherModule.coordinates N)
    tryGrowAllCoordinates 


// Keep growing segments as above until no further merging is possible
let createGrowUntilNoChangeFunction (tryGrowAllCoordinates:Segmentation->Segmentation) : (Segmentation->Segmentation) =
    let rec growUntilNoChange (segmentation: Segmentation) : Segmentation =
        let segmentationGrowAll = tryGrowAllCoordinates segmentation
        let segentationGrowForAll = tryGrowAllCoordinates segmentationGrowAll
        if (segmentationGrowAll = segmentation) then
            segmentationGrowAll
        else
            segentationGrowForAll
    growUntilNoChange

// Segment the given image based on the given merge cost threshold, but only for the top left corner of the image of size (2^N x 2^N)
let segment (image:TiffModule.Image) (N: int) (threshold:float)  : (Coordinate -> Segment) =
    let startSegmentation = Map.empty
    let segmentPixelMap = createPixelMap image
    let allNeighbours = createNeighboursFunction segmentPixelMap N
    let bestOfAllNeighbours = createBestNeighbourFunction allNeighbours threshold
    let growOneSegmentforBest = createTryGrowOneSegmentFunction bestOfAllNeighbours segmentPixelMap
    let growAllCoordinatesforBest = createTryGrowAllCoordinatesFunction growOneSegmentforBest N
    let segmentationLast = growAllCoordinatesforBest startSegmentation

    let findSegmentation (coord : Coordinate) : Segment=
        let lastPixelMap = segmentPixelMap coord
        findRoot segmentationLast lastPixelMap
    findSegmentation