(*
This module implements wires between symbol ports. Wires can be autorouted, or manually routed by dragging segments.
Moving symbols causes the corresponding wires to move.
Wires are read and written from Issie as lists of wire vertices, whatever teh internal representation is.
*)


module BusWire

open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers

//Static Vars
let minSegLen = 5.

//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

///
type Orientation =  Horizontal | Vertical

///
type SnapPosition = High | Mid | Low

type ASeg =
     {
         Id: SegmentId
         Start: XYPos
         Length: float // positive or negative
         Dir: Orientation // horizontal or vertical
         ManualRoute: bool // true for manual-route, false for auto route
         HostId: ConnectionId
         JumpCoordinateList: list<float * SegmentId>
         Draggable : bool
     }

///Used for displaying wires
type WireStyle =
    | Classic
    | Radiused
        
type Wire =
     {
         Id: ConnectionId 
         InputPort: InputPortId
         OutputPort: OutputPortId
         Color: HighLightColor
         Width: int
         Segments: ASeg array
     }
     with static member stickLength = 16.0

///
type Model =
    {
        Symbol: Symbol.Model
        WX: Map<ConnectionId, Wire>
        FromVerticalToHorizontalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        FromHorizontalToVerticalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        CopiedWX: Map<ConnectionId, Wire> 
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: list<ConnectionId>
        Notifications: Option<string>
        Style: WireStyle
    }

///Returns segment End coordinates
let segmentEndCoords (segment: ASeg) : XYPos =
    match segment.Dir with
    | Horizontal -> {X = segment.Start.X + segment.Length; Y = segment.Start.Y}
    | Vertical -> {X = segment.Start.X; Y = segment.Start.Y + segment.Length}

///Returns segment Index
let segmentIndex (segment: ASeg) (model: Model): int =
    let wire = model.WX.[segment.HostId]

    wire.Segments
    |> Array.findIndex (fun seg -> seg.Id = segment.Id)

//----------------------------Message Type-----------------------------------//

///
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (InputPortId * OutputPortId)
    | BusWidths
    | CopyWires of list<ConnectionId>
    | DeleteWires of list<ConnectionId>
    | SelectWires of list<ConnectionId>
    | UpdateWires of list<ComponentId> * XYPos
    | UpdateStyle of WireStyle
    | DragWire of ConnectionId * MouseT
    | ColorWires of list<ConnectionId> * HighLightColor
    | ErrorWires of list<ConnectionId>
    | ResetJumps
    | MakeJumps 
    | ResetModel // For Issie Integration
    | LoadConnections of list<Connection> // For Issie Integration

//-------------------------Debugging functions---------------------------------//
let ppSId (sId:SegmentId) =
    sId
    |> (fun (SegmentId x) -> x)
    |> Seq.toList
    |> (fun chars -> chars.[0..2])
    |> List.map string
    |> String.concat ""

let ppS (seg:ASeg) =
    sprintf $"|{ppSId seg.Id}|"

let ppWId (wId:ConnectionId) =
        wId
        |> (fun (ConnectionId x) -> x)
        |> Seq.toList
        |> (fun chars -> chars.[0..2])
        |> List.map string
        |> String.concat ""

let ppMaps (model:Model) =
    let mhv = model.FromHorizontalToVerticalSegmentIntersections
    let mvh = model.FromVerticalToHorizontalSegmentIntersections
    let m1 =
        mhv
        |> Map.toList
        |> List.map (fun (sid,lst) ->
            List.map (snd >> ppSId) lst
            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
            |> String.concat ";\n"
    let m2 =
        mvh
        |> Map.toList
        |> List.map (fun (sid,lst) ->
            List.map (snd >> ppSId) lst
            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
            |> String.concat ";\n"
    let jumps =
        model.WX
        |> Map.toList
        |> List.map (fun (wId,w) ->
            sprintf $"Wire: {w.Segments |> Array.toList |>List.collect (fun seg -> seg.JumpCoordinateList |> List.map (fun (f, sid) -> ppSId sid))}")
            
    printfn $"\n------------------\nMapHV:\n {m1} \n MapVH\n{m2} \nJumps:\n {jumps}\n------------------\n"



let ppSeg seg (model: Model) = 
        let cid,sid = seg
        let wire = model.WX.[cid]
        let sg = Array.find (fun (s:ASeg) -> s.Id = sid ) wire.Segments
        let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
        sprintf $"""[{ppSId sg.Id}: {pxy sg.Start}->{pxy (segmentEndCoords sg)}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}"""

let pp segs (model: Model)= 
    segs
    |> List.map  ( fun seg ->
        let cid,sid = seg
        let wire = model.WX.[cid]
        match Array.tryFind (fun (s:ASeg) -> s.Id = sid ) wire.Segments with
        | Some  sg ->
            let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
            sprintf $"""[{pxy sg.Start}->{pxy (segmentEndCoords sg)}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}"""
        | None -> "XX")
    |> String.concat ";"

///
let MapToSortedList map : Wire list = 
    let listSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Purple) map
        |> Map.toList
        |> List.map snd
    let listErrorSelected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Brown) map
        |> Map.toList
        |> List.map snd
    let listErrorUnselected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Red) map
        |> Map.toList
        |> List.map snd
    let listUnSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.DarkSlateGrey) map
        |> Map.toList
        |> List.map snd
    let listCopied = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Thistle) map
        |> Map.toList
        |> List.map snd
    let listWaves = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Blue) map
        |> Map.toList
        |> List.map snd

    listUnSelected @ listErrorUnselected @ listErrorSelected @ listSelected @ listWaves @ listCopied

//-------------------------------Implementation code----------------------------//

/// Given the two Symbol port sides, returns one of 28 port side case numbers
let getPortSideCase (portCoords : XYPos * XYPos) (portSides: Symbol.Side * Symbol.Side) =
    let xs_noStick, ys_noStick, xt_noStick, yt_noStick = snd(portCoords).X, snd(portCoords).Y, fst(portCoords).X, fst(portCoords).Y

    let xs, ys =
        match snd(portSides) with
        | Symbol.Side.R -> xs_noStick + Wire.stickLength, ys_noStick
        | Symbol.Side.L -> xs_noStick - Wire.stickLength, ys_noStick   
        | Symbol.Side.B -> xs_noStick, ys_noStick + Wire.stickLength
        | Symbol.Side.T -> xs_noStick, ys_noStick - Wire.stickLength

    let xt, yt =
        match fst(portSides) with   
        | Symbol.Side.R -> xt_noStick + Wire.stickLength, yt_noStick 
        | Symbol.Side.L -> xt_noStick - Wire.stickLength, yt_noStick
        | Symbol.Side.B -> xt_noStick, yt_noStick + Wire.stickLength 
        | Symbol.Side.T -> xt_noStick, yt_noStick - Wire.stickLength

    match portSides with
        // LR
        | Symbol.Side.L, Symbol.Side.R -> 
            if xs < xt then
                0 
            else
                1 
        // RL
        | Symbol.Side.R, Symbol.Side.L -> 
            if xs > xt then
                2 
            else
                3 
        // TB (Equivalent to LR rotated 90 deg clockwise)
        | Symbol.Side.T, Symbol.Side.B -> 
            if ys < yt then
                4 
            else
                5 
        // BT (Equivalent to RL rotated 90 deg clockwise)
        | Symbol.Side.B, Symbol.Side.T -> 
            if ys > yt then
                6 
            else
                7 
        // RR, LL, BB, TT
        | Symbol.Side.R, Symbol.Side.R -> 
            8
        | Symbol.Side.L, Symbol.Side.L -> 
            9
        | Symbol.Side.B, Symbol.Side.B -> 
            10
        | Symbol.Side.T, Symbol.Side.T -> 
            11
        // TR
        | Symbol.Side.T, Symbol.Side.R -> 
            if xs < xt && ys < yt then 
                12
            else 
                13
        // RT
        | Symbol.Side.R, Symbol.Side.T -> 
            if xs > xt && ys > yt then 
                14
            else
                15
        // BL
        | Symbol.Side.B, Symbol.Side.L ->
            if xs > xt && ys > yt then 
                16
            else 
                17
        // LB
        | Symbol.Side.L, Symbol.Side.B -> 
            if xs < xt && ys < yt then 
                18
            else
                19
        // RB 
        | Symbol.Side.R, Symbol.Side.B -> 
            if xs > xt && ys < yt then 
                20
            else 
                21
        // BR 
        | Symbol.Side.B, Symbol.Side.R -> 
            if xs < xt && ys > yt then 
                22
            else
                23
        // LT
        | Symbol.Side.L, Symbol.Side.T -> 
            if xs < xt && ys > yt then // 2 segs
                24
            else // 4 segs 
                25
        // TL
        | Symbol.Side.T, Symbol.Side.L -> 
            if ys < yt && xs > xt then 
                26
            else
                27


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// wire vertices
let makeInitialWireVerticesList (portCoords : XYPos * XYPos)  (portSides: Symbol.Side * Symbol.Side )= 
    let xs, ys, xt, yt = snd(portCoords).X, snd(portCoords).Y, fst(portCoords).X, fst(portCoords).Y
    let portSideCase = getPortSideCase portCoords portSides

    // adjust length of segments 0 and 6
    let hStickLen = Wire.stickLength
    let vStickLen = Wire.stickLength

    // printfn$"Port side case number: {portSideCase}"

    let xArr, yArr =
        match portSideCase with
        // LR
        | 0 -> 
            [|xs; xs+hStickLen; xs+hStickLen; (xs+xt)/2.0; (xs+xt)/2.0; xt-hStickLen; xt-hStickLen; xt|], 
            [|ys; ys; ys; ys; yt; yt; yt; yt|]
        | 1 ->
            [|xs; xs+hStickLen; xs+hStickLen; xs+hStickLen; xt-hStickLen; xt-hStickLen; xt-hStickLen; xt|], 
            [|ys; ys; ys; (ys+yt)/2.0; (ys+yt)/2.0; yt; yt; yt|]
        // RL
        | 2 -> 
            [|xs; xs-hStickLen; xs-hStickLen; (xs+xt)/2.0; (xs+xt)/2.0; xt+hStickLen; xt+hStickLen; xt|], 
            [|ys; ys; ys; ys; yt; yt; yt; yt|]
        | 3 -> 
             [|xs; xs-hStickLen; xs-hStickLen; xs-hStickLen; xt+hStickLen; xt+hStickLen; xt+hStickLen; xt|], 
             [|ys; ys; ys; (ys+yt)/2.0; (ys+yt)/2.0; yt; yt; yt|]
        // TB (Equivalent to LR rotated 90 deg clockwise)
        | 4 -> 
            [|xs; xs; xs; xs; xt; xt; xt; xt|],
            [|ys; ys+vStickLen; ys+vStickLen; (ys+yt)/2.0; (ys+yt)/2.0; yt-vStickLen; yt-vStickLen; yt|] 
        | 5 -> 
            [|xs; xs; xs; (xs+xt)/2.0; (xs+xt)/2.0; xt; xt; xt|],
            [|ys; ys+vStickLen; ys+vStickLen; ys+vStickLen; yt-vStickLen; yt-vStickLen; yt-vStickLen; yt|]
        // BT (Equivalent to RL rotated 90 deg clockwise)
        | 6 -> 
            [|xs; xs; xs; xs; xt; xt; xt; xt|],
            [|ys; ys-vStickLen; ys-vStickLen; (ys+yt)/2.0; (ys+yt)/2.0; yt+vStickLen; yt+vStickLen; yt|]
        | 7 ->
            [|xs; xs; xs; (xs+xt)/2.0; (xs+xt)/2.0; xt; xt; xt|],
            [|ys; ys-vStickLen; ys-vStickLen; ys-vStickLen; yt+vStickLen; yt+vStickLen; yt+vStickLen; yt|]
        // RR, LL, BB, TT
        | 8 -> 
            [|xs; xs+hStickLen; xs+hStickLen; max (xs+hStickLen) (xt+hStickLen); max (xs+hStickLen) (xt+hStickLen); xt+hStickLen; xt+hStickLen; xt|], 
            [|ys; ys; ys; ys; yt; yt; yt; yt|]
        | 9 -> 
            [|xs; xs-hStickLen; xs-hStickLen; min (xs-hStickLen) (xt-hStickLen); min (xs-hStickLen) (xt-hStickLen); xt-hStickLen; xt-hStickLen; xt|], 
            [|ys; ys; ys; ys; yt; yt; yt; yt|]
        | 10 -> 
            [|xs; xs; xs; xs; xt; xt; xt; xt|], 
            [|ys; ys+vStickLen; ys+vStickLen; max (ys+vStickLen) (yt+vStickLen); max (ys+vStickLen) (yt+vStickLen); yt+vStickLen; yt+vStickLen; yt|]
        | 11 -> 
            [|xs; xs; xs; xs; xt; xt; xt; xt|], 
            [|ys; ys-vStickLen; ys-vStickLen; min (ys-vStickLen) (yt-vStickLen); min (ys-vStickLen) (yt-vStickLen); yt-vStickLen; yt-vStickLen; yt|]
        // TR
        | 12 -> 
            [|xs; xs+hStickLen; xs+hStickLen; xt; xt; xt; xt; xt|],
            [|ys; ys; ys; ys; yt-vStickLen; yt-vStickLen; yt-vStickLen; yt|]
        | 13 -> 
            [|xs; xs+hStickLen; xs+hStickLen; xs+hStickLen; xt; xt; xt; xt|],
            [|ys; ys; ys; yt-vStickLen; yt-vStickLen; yt-vStickLen; yt-vStickLen; yt|]
        // RT
        | 14 -> 
            [|xs; xs; xs; xs; xs; xt+hStickLen; xt+hStickLen; xt|],
            [|ys; ys-vStickLen; ys-vStickLen; ys-vStickLen; yt; yt; yt; yt|]
        | 15 ->         
            [|xs; xs; xs; xs; xt+hStickLen; xt+hStickLen; xt+hStickLen; xt|],
            [|ys; ys-vStickLen; ys-vStickLen; ys-vStickLen; ys-vStickLen; yt; yt; yt|]
        // BL
        | 16 ->
            [|xs; xs-hStickLen; xs-hStickLen; xt; xt; xt; xt; xt|],
            [|ys; ys; ys; ys; yt+vStickLen; yt+vStickLen; yt+vStickLen; yt|]
        | 17 ->
            [|xs; xs-hStickLen; xs-hStickLen; xs-hStickLen; xt; xt; xt; xt|],
            [|ys; ys; ys; yt+vStickLen; yt+vStickLen; yt+vStickLen; yt+vStickLen; yt|]
        // LB
        | 18 ->
            [|xs; xs; xs; xs; xs; xt-hStickLen; xt-hStickLen; xt|],
            [|ys; ys+vStickLen; ys+vStickLen; ys+vStickLen; yt; yt; yt; yt|]
        | 19 ->
            [|xs; xs; xs; xs; xt-hStickLen; xt-hStickLen; xt-hStickLen; xt|],
            [|ys; ys+vStickLen; ys+vStickLen; ys+vStickLen; ys+vStickLen; yt; yt; yt|]
        // RB 
        | 20-> 
            [|xs; xs; xs; xs; xt+hStickLen; xt+hStickLen; xt+hStickLen; xt|],
            [|ys; ys+vStickLen; ys+vStickLen; yt; yt; yt; yt; yt|]
        | 21 ->
            [|xs; xs; xs; xt+hStickLen; xt+hStickLen; xt+hStickLen; xt+hStickLen; xt|],
            [|ys; ys+vStickLen; ys+vStickLen; ys+vStickLen; yt; yt; yt; yt|]
        // BR 
        | 22 ->
            [|xs; xs+hStickLen; xs+hStickLen; xs+hStickLen; xt; xt; xt; xt|],
            [|ys; ys; ys; ys; ys; yt+vStickLen; yt+vStickLen; yt|]
        | 23 ->
            [|xs; xs+hStickLen; xs+hStickLen; xs+hStickLen; xs+hStickLen; xt; xt; xt|],
            [|ys; ys; ys; ys; yt+vStickLen; yt+vStickLen; yt+vStickLen; yt|]
        // LT
        | 24 ->
            [|xs; xs; xs; xs; xt-hStickLen; xt-hStickLen; xt-hStickLen; xt|],
            [|ys; ys-vStickLen; ys-vStickLen; yt; yt; yt; yt; yt|]
        | 25 -> 
            [|xs; xs; xs; xt-hStickLen; xt-hStickLen; xt-hStickLen; xt-hStickLen; xt|],
            [|ys; ys-vStickLen; ys-vStickLen; ys-vStickLen; yt; yt; yt; yt|]
        // TL
        | 26 -> 
            [|xs; xs-hStickLen; xs-hStickLen; xs-hStickLen; xt; xt; xt; xt|],
            [|ys; ys; ys; ys; ys; yt-vStickLen; yt-vStickLen; yt|]
        | 27 -> 
            [|xs; xs-hStickLen; xs-hStickLen; xs-hStickLen; xs-hStickLen; xt; xt; xt|],
            [|ys; ys; ys; ys; yt-vStickLen; yt-vStickLen; yt-vStickLen; yt|]
        | _ -> failwithf$"Invalid port side case number {portSideCase} encountered!"
        
    [
        {X = xArr[0]; Y = yArr[0]};
        {X = xArr[1]; Y = yArr[1]};
        {X = xArr[2]; Y = yArr[2]};
        {X = xArr[3]; Y = yArr[3]};
        {X = xArr[4]; Y = yArr[4]};
        {X = xArr[5]; Y = yArr[5]};
        {X = xArr[6]; Y = yArr[6]};
        {X = xArr[7]; Y = yArr[7]};
    ],
    portSideCase

///
let inferDirectionfromVertices (xyVerticesList: XYPos list) =
    if xyVerticesList.Length <> 8 then 
        failwithf $"Can't perform connection type inference except with 8 vertices: here given {xyVerticesList.Length} vertices"

    let getDir (vs:XYPos) (ve:XYPos) =
        match sign ((abs vs.X - abs ve.X)*(abs vs.X - abs ve.X) - (abs vs.Y - abs ve.Y)*(abs vs.Y - abs ve.Y)) with
        | 1 -> Some Horizontal
        | -1 -> Some Vertical
        | _ -> None

    let midS, midE = xyVerticesList.[3], xyVerticesList.[4]
    let first,last = xyVerticesList.[1], xyVerticesList.[5]
    let xDelta = abs last.X - abs first.X

    match getDir midS midE, abs xDelta > 20.0, xDelta > 0.0 with
    | Some Horizontal, _, _ when midE.X < midS.X -> Some Horizontal
    | Some Vertical, _, _ -> Some Vertical 
    | _, true, true -> Some Vertical
    | _, true, false -> Some Horizontal
    | _, false, _ -> None

/// Turns a list of vertices into a list of segments
let xyVerticesToSegments connId (portSideCase: int) (xyVerticesList: XYPos list)  =
    // printfn "%A" (portSideCase)
    let dirs = 
        match portSideCase with
        | 0 | 2 -> 
            // (LR/RL) - 3 seg wire, 5 adjustable segments
            [Horizontal; Vertical; Horizontal; Vertical; Horizontal; Vertical; Horizontal]
        | 1 | 3 ->
            // (LR/RL) - 5 seg wire, 3 adjustable segments
            [Horizontal; Horizontal; Vertical; Horizontal; Vertical; Horizontal; Horizontal]
        | 4 | 6 ->
            // (TB/BT) - 3 seg wire, 5 adjustable segments
            [Vertical; Horizontal; Vertical; Horizontal; Vertical; Horizontal; Vertical]
        | 5 | 7 ->
            // (TB/BT) - 5 seg wire, 3 adjustable segments
            [Vertical; Vertical; Horizontal; Vertical; Horizontal; Vertical; Vertical]
        | 8 | 9 ->
            // (RR/LL) - 3 seg wire
            [Horizontal; Vertical; Horizontal; Vertical; Horizontal; Vertical; Horizontal]
        | 10 | 11 ->
            // (BB/TT) - 3 seg wire
            [Vertical; Horizontal; Vertical; Horizontal; Vertical; Horizontal; Vertical]
        | 12 | 16 ->
            // (TR/BL) - 2 seg wire
            [Horizontal; Vertical; Horizontal; Vertical; Horizontal; Horizontal; Vertical]
        | 13 | 17 ->
            // (TR/BL) - 4 seg wire
            [Horizontal; Horizontal; Vertical; Horizontal; Vertical; Vertical; Vertical]
        | 14 | 18 -> 
            // (RT/LB) - 2 seg wire
            [Vertical; Horizontal; Horizontal; Vertical; Horizontal; Vertical; Horizontal]
        | 15 | 19 ->
            // (RT/LB) - 4 seg wire
            [Vertical; Vertical; Vertical; Horizontal; Vertical; Horizontal; Horizontal]
        | 20 | 24 -> 
            // (RB/LT) - 2 seg wire
            [Vertical; Horizontal; Vertical; Horizontal; Vertical; Vertical; Horizontal]
        | 21 | 25 -> 
            // (RB/LT) - 4 seg wire
            [Vertical; Vertical; Horizontal; Vertical; Horizontal; Horizontal; Horizontal]
        | 22 | 26 -> 
            // (BR/TL) - 2 seg wire
            [Horizontal; Vertical; Vertical; Horizontal; Vertical; Horizontal; Vertical]
        | 23 | 27 -> 
            // (BR/TL) - 4 seg wire
            [Horizontal; Horizontal; Horizontal; Vertical; Horizontal; Vertical; Vertical]
        | _ -> 
            [Horizontal; Vertical; Horizontal; Vertical; Horizontal; Vertical; Horizontal]

    List.pairwise xyVerticesList
    |> List.mapi (
        fun i ({X=startX; Y=startY},{X=endX; Y=endY}) ->    
            {
                Id = SegmentId(JSHelpers.uuid())
                Start = {X=startX;Y=startY};
                Length = endX - startX + endY - startY;
                Dir = dirs.[i]
                ManualRoute = false;
                HostId  = connId;
                JumpCoordinateList = [];
                Draggable =
                    match i with
                    | 1 | 5 ->  (portSideCase % 2 = 0)
                    | 0  | 6  -> false
                    | _ -> true
            })
     |> List.toArray

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments 
        (connId) 
        (verticesList: list<float*float>) =
    let xyVerticesList =
        verticesList
        |> List.map (fun (x,y) -> {X=x;Y=y})

    let makeSegmentsFromVertices (xyList: XYPos list) =
        makeInitialWireVerticesList (xyList.[0], xyList.[xyList.Length - 1]) (Symbol.Side.R, Symbol.Side.L)
        |> (fun (vl, isLeftToRight) -> xyVerticesToSegments connId isLeftToRight vl)

    // segments lists must must be length 7, in case legacy vertex list does not conform check this
    // if there are problems reroute
    //vertex lists are one element longer than segment lists
    if xyVerticesList.Length <> 8 then  
        makeSegmentsFromVertices xyVerticesList
    else 
        match inferDirectionfromVertices xyVerticesList with
        | Some Vertical -> 
            printfn "Converting vertical"
            xyVerticesToSegments connId 0 xyVerticesList
        | Some Horizontal -> 
            printfn "Converting horizontal"
            xyVerticesToSegments connId 1 xyVerticesList
        | _ ->
            // can't work out what vertices are, so default to auto-routing
            printfn "Converting unknown"
            makeSegmentsFromVertices xyVerticesList
 
//----------------------interface to Issie-----------------------//

/// This function is given a ConnectionId and it outputs a Connection.
/// Used to interface with Issie.
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let segmentsToVertices (segArr:ASeg array) = 
        let firstCoord = (segArr.[0].Start.X, segArr.[0].Start.Y)
        let verticesExceptFirst = segArr
                                  |> Array.toList
                                  |> List.mapi (fun i seg -> match seg.Dir with
                                                              | Horizontal -> (seg.Start.X + seg.Length), seg.Start.Y
                                                              | Vertical -> seg.Start.X, (seg.Start.Y + seg.Length) )


        [firstCoord] @ verticesExceptFirst

    let conn = wModel.WX.[cId]
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = conn.Id, conn.InputPort, conn.OutputPort

    {
        Id = strId
        Source = { Symbol.getPort wModel.Symbol strOutputPort with PortNumber = None } // None for connections 
        Target = { Symbol.getPort wModel.Symbol strInputPort with PortNumber = None } // None for connections 
        Vertices = segmentsToVertices conn.Segments
    }

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connection, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> =
    wModel.WX
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)

/// Given three points p, q, r, the function returns true if 
/// point q lies on line segment 'pr'. Otherwise it returns false.
let onSegment (p : XYPos) (q : XYPos) (r : XYPos) : bool = 
    (
        (q.X <= max (p.X) (r.X)) &&
        (q.X >= min (p.X) (r.X)) &&
        (q.Y <= max (p.Y) (r.Y)) &&
        (q.Y >= min (p.Y) (r.Y))
    )
  
/// Given three points p, q, r, the function returns:
/// - 0 if p, q and r are colinear;
/// - 1 if the path that you must follow when you start at p, you visit q and you end at r, is a CLOCKWISE path;
/// - 2 if the path that you must follow when you start at p, you visit q and you end at r, is a COUNTERCLOCKWISE path.
let orientation (p : XYPos) (q : XYPos) (r : XYPos) : int =
    let result = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
  
    if (result = 0.0) then 0 // colinear
    elif (result > 0.0) then 1 // clockwise
    else 2 //counterclockwise

///Returns the abs of an XYPos object
let getAbsXY (pos : XYPos) = 
    {X = abs pos.X; Y = abs pos.Y}
  
/// Given two sets of two points: (p1, q1) and (p2, q2)
/// that define two segments, the function returns true
/// if these two segments intersect and false otherwise.
let segmentIntersectsSegment ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : bool =
    // this is a terrible implementation
    // determining intersection should be done by finding intersection point and comparing with coords
    // since segments are always horizontal or vertical that is pretty easy.
    // in addition the way that coordinates can be positive or negative but are absed when used is appalling
    // the manual or auto route info per segment should be a separate field in Segmnet, not encoded in the sign of the coordinates
    // that is needed when writing out or reading from Issie, but the write/read process can easily translate to a same internal data structure in the draw block model
    // !!! - kept for now, as it's also used in part 2

    let p1,q1,p2,q2= getAbsXY p1, getAbsXY q1, getAbsXY p2, getAbsXY q2
    // Find the four orientations needed for general and 
    // special cases 
    let o1 = orientation (p1) (q1) (p2)
    let o2 = orientation (p1) (q1) (q2)
    let o3 = orientation (p2) (q2) (p1)
    let o4 = orientation (p2) (q2) (q1)
  
    // General case 
    if (o1 <> o2 && o3 <> o4)
        then true

    // Special Cases 
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1 
    elif (o1 = 0 && onSegment (p1) (p2) (q1))
        then true
  
    // p1, q1 and q2 are colinear and q2 lies on segment p1q1 
    elif (o2 = 0 && onSegment (p1) (q2) (q1))
        then true
  
    // p2, q2 and p1 are colinear and p1 lies on segment p2q2 
    elif (o3 = 0 && onSegment (p2) (p1) (q2))
        then true
  
     // p2, q2 and q1 are colinear and q1 lies on segment p2q2 
    elif (o4 = 0 && onSegment (p2) (q1) (q2))
        then true
    else false


/// Given two coordinates, this function returns the euclidean
/// distance between them.
/// - Same as function euclideanDistance in DrawHelpers
let distanceBetweenTwoPoints (pos1 : XYPos) (pos2 : XYPos) : float =
    sqrt ( (pos1.X - pos2.X)*(pos1.X - pos2.X) + (pos1.Y - pos2.Y)*(pos1.Y - pos2.Y) )


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns an array of
/// Segment(s).
let makeInitialSegmentsList (hostId : ConnectionId) (portCoords : XYPos * XYPos) (portSides: Symbol.Side * Symbol.Side): ASeg array =
    let xyPairs, portSideCase = makeInitialWireVerticesList portCoords portSides
    xyPairs
    |> xyVerticesToSegments hostId portSideCase

/// This function renders the given
/// segment (i.e. creates a ReactElement
/// using the data stored inside it),
/// using the colour and width properties given.
let renderSegment (segment : ASeg) (colour : string) (width : string) : ReactElement = 
    let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match wOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let halfWidth = (renderWidth/2.0) - (0.75)
    let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }

    if segment.Dir = Horizontal then
        let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

        let renderWireSubSegment (vertex1 : XYPos) (vertex2 : XYPos) : list<ReactElement> =
            let Xa, Ya, Xb, Yb = vertex1.X, vertex1.Y, vertex2.X, vertex2.Y
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]
        
        let segmentJumpHorizontalSize = 9.0
        let segmentJumpVerticalSize = 6.0
        
        let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
            let x, y = intersectionCoordinate.X, intersectionCoordinate.Y

            let startingPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y}
            let startingControlPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
            let endingControlPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
            let endingPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y}

            makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
            ::
            makeCircle startingPoint.X startingPoint.Y circleParameters
            ::
            [
                makeCircle endingPoint.X endingPoint.Y circleParameters
            ]
        
        let rec renderMultipleSegmentJumps (segmentJumpCoordinateList : list<float>) (segmentJumpYCoordinate : float) : list<ReactElement> =
            match segmentJumpCoordinateList with
            | [] -> []

            | [singleElement] ->
                renderSingleSegmentJump {X = singleElement; Y = segmentJumpYCoordinate}

            | firstElement :: secondElement :: tailList ->

                if segment.Length < 0. then
                    renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                    @
                    renderWireSubSegment {X = firstElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                    @
                    renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
                
                else
                    renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                    @
                    renderWireSubSegment {X = firstElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                    @
                    renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
            

        let completeWireSegmentRenderFunction (seg : ASeg) : list<ReactElement> =
            let jumpCoordinateList =
                if segment.Length < 0. then
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sortDescending 
                else
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sort
            
            match jumpCoordinateList with
                | [] -> renderWireSubSegment seg.Start (segmentEndCoords seg)
                | lst ->
                     let y = seg.Start.Y // SHOULD be equal to seg.End.Y since ONLY horizontal segments have jumps
                     let firstSegmentJumpCoordinate = lst.[0]
                     let lastSegmentJumpCoordinate = lst.[(List.length lst) - 1]

                     if segment.Length < 0. then
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y} (segmentEndCoords seg)

                     else
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y} (segmentEndCoords seg)
        

        let wireSegmentReactElementList = segment
                                          |> completeWireSegmentRenderFunction

        g [] wireSegmentReactElementList
    
    else
        let segEnd = segmentEndCoords segment
        let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, segEnd.X, segEnd.Y
        let segmentElements = 
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
               makeCircle Xb Yb circleParameters
            ]
        g [] segmentElements


///Renders a radiused style wire
let renderWireRadiused (segments : ASeg array) (colour : string) (width : string) : ReactElement list =
    //Setting render parameters
    let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match wOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let halfWidth = (renderWidth/2.0) - (0.75)
    let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }
    let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }


    // Renders a single segment and/or the radiused path to the previous segment
    let renderSegmentRadiused (segment: ASeg) : ReactElement =
        let index = segments |> Array.findIndex (fun elm -> elm = segment)
        let segEnd = segmentEndCoords segment
        let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, segEnd.X, segEnd.Y
        let referenceRadVal = 25.

        // Take 50 % radius if referenceRadVal is greater than half the length of current segment
        let dX = 
            if referenceRadVal < (0.5 * abs (Xb - Xa)) then
                referenceRadVal * (float (System.Math.Sign (Xb - Xa)))
            else
                0.5 * (Xb - Xa)
        let dY =
            if referenceRadVal < (0.5 * abs (Yb - Ya)) then
                referenceRadVal * (float (System.Math.Sign (Yb - Ya)))
            else
                0.5 * (Yb - Ya)

        let segmentElements =
            if index = 0 then
                [makeLine Xa Ya (Xb - dX) (Yb - dY) lineParameters]
            else
                let prevSeg = segments.[index-1]
                let prevSegEnd = segmentEndCoords prevSeg
                let prevXa, prevYa, prevXb, prevYb = prevSeg.Start.X, prevSeg.Start.Y, prevSegEnd.X, prevSegEnd.Y

                // Take 50 % radius if referenceRadVal is greater than half the length of previous segment
                let prevDX = 
                    if referenceRadVal < (0.5 * abs (prevXb - prevXa)) then
                        referenceRadVal * (float (System.Math.Sign (prevXb - prevXa)))
                    else
                        0.5 * (prevXb - prevXa)
                let prevDY =
                    if referenceRadVal < (0.5 * abs (prevYb - prevYa)) then
                        referenceRadVal * (float (System.Math.Sign (prevYb - prevYa)))
                    else
                        0.5 * (prevYb - prevYa)
                    
                // control points are always kept at joining point of two sements
                let startingPoint = {X = prevXb - prevDX; Y = prevYb - prevDY}
                let startingControlPoint = {X = prevXb; Y = prevYb}
                let endingControlPoint = {X = Xa; Y = Ya}
                let endingPoint = {X = Xa + dX; Y = Ya + dY}

                if index = 6 then
                    if segment.Dir = prevSeg.Dir then
                        makeLine (prevXb - prevDX) (prevYb - prevDY) prevXb prevYb lineParameters
                        ::
                        [makeLine Xa Ya Xb Yb lineParameters]
                    else
                        makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
                        ::
                        [makeLine (Xa + dX) (Ya + dY) Xb Yb lineParameters]
                elif index < 6 then
                    if segment.Dir = prevSeg.Dir then
                        makeLine (prevXb - prevDX) (prevYb - prevDY) prevXb prevYb lineParameters
                        ::
                        [makeLine Xa Ya (Xb - dX) (Yb - dY) lineParameters]
                    else
                        makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
                        ::
                        [makeLine (Xa + dX) (Ya + dY) (Xb - dX) (Yb - dY) lineParameters]
                else
                    failwithf "Segment array illegally large"


        g [] segmentElements

    segments
    |> Array.map renderSegmentRadiused
    |> Array.toList


type WireRenderProps =
    {
        key: string
        Segments: ASeg array
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortLocation: XYPos
        Style: WireStyle
        OutputPortSide: Symbol.Side
    }

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let renderWireWidthText : ReactElement =
                let textParameters =
                    {
                        TextAnchor = "left";
                        FontSize = "12px";
                        FontWeight = "Bold";
                        FontFamily = "Verdana, Arial, Helvetica, sans-serif";
                        Fill = props.ColorP.Text();
                        UserSelect = UserSelectOptions.None;
                        DominantBaseline = "middle";
                    }
                let textString = if props.StrokeWidthP = 1 then "" else string props.StrokeWidthP //Only print width > 1
                match props.OutputPortSide with
                | Symbol.Side.R -> makeText (props.OutputPortLocation.X+1.0) (props.OutputPortLocation.Y-7.0) (textString) textParameters
                | Symbol.Side.B -> makeText (props.OutputPortLocation.X+7.0) (props.OutputPortLocation.Y+1.0) (textString) textParameters
                | Symbol.Side.L -> makeText (props.OutputPortLocation.X-4.0) (props.OutputPortLocation.Y-7.0) (textString) textParameters
                | Symbol.Side.T -> makeText (props.OutputPortLocation.X+7.0) (props.OutputPortLocation.Y-1.0) (textString) textParameters

            let renderWireSegmentList : list<ReactElement> =
                match props.Style with
                | Classic ->
                            props.Segments
                            |> Array.map
                                (
                                    fun (segment : ASeg) -> renderSegment segment (props.ColorP.Text()) (string props.StrokeWidthP)
                                )
                            |> Array.toList
                | Radiused ->
                            renderWireRadiused props.Segments (props.ColorP.Text()) (string props.StrokeWidthP)
                    
            g [] ([ renderWireWidthText ] @ renderWireSegmentList)
        
    , "Wire"
    , equalsButFunctions
    )

   
let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()

    let wires =
        model.WX
        |> Map.toArray
        |> Array.map snd
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId
                        
                    let outputPortLocation = Symbol.getOnePortLocationNew model.Symbol stringOutId PortType.Output
                    let outputPortSide = Symbol.getOnePortSide model.Symbol stringOutId PortType.Output
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            Segments = wire.Segments
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                            Style = model.Style
                            OutputPortSide = outputPortSide
                        }

                    singleWireView props )

    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start


/// This function is given two couples of
/// points that define two line segments and it returns:
/// - Some (x, y) if the two segments intersect;
/// - None if the do not.
let segmentIntersectsSegmentCoordinates ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : Option<XYPos> =
    
    if (segmentIntersectsSegment (p1, q1) (p2, q2)) then
        let x1, y1, x2, y2 = abs p1.X, abs p1.Y, abs q1.X, abs q1.Y
        let x3, y3, x4, y4 = abs p2.X, abs p2.Y, abs q2.X, abs q2.Y
        let uA = ((x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)) / ((y4-y3)*(x2-x1) - (x4-x3)*(y2-y1))

        let intersectionX = x1 + (uA * (x2-x1)) // if coordinates are wanted, maybe useful later
        let intersectionY = y1 + (uA * (y2-y1))
        Some {X = intersectionX; Y = intersectionY}
    
    else None

/// This funtion is given a bounding box and it returns the coordinates
/// of the top-left and the bottom-right corners of this bounding box.
let getTopLeftAndBottomRightCorner (box : BoundingBox) : XYPos * XYPos = 
    let {BoundingBox.X = x; BoundingBox.Y = y} = box
    let {BoundingBox.H = h; BoundingBox.W = w} = box
    let coords = [(x, y); (x, y+h); (x+w, y); (x+w, y+h)]
    let topLeft = List.min coords
    let bottomRight = List.max coords

    {X = fst(topLeft) ; Y = snd(topLeft)} , {X = fst(bottomRight) ; Y = snd(bottomRight)}

/// This function is given a Segment and a BoundingBox
/// and it returns:
/// - (false, None) if the segment does not intersect the bounding box
/// - (true, None) if the segment is fully included inside the bounding box
/// - (true, Some coordinate)  if the segment intersects the bounding box
let segmentIntersectsBoundingBoxCoordinates (seg : ASeg) (bb : BoundingBox) : bool =
    
    let leftX, rightX, topY, bottomY = bb.X, (bb.X + bb.W), bb.Y, (bb.Y + bb.H)
    let segEnd = segmentEndCoords seg
    let x1, y1, x2, y2 = seg.Start.X, seg.Start.Y, segEnd.X, segEnd.Y 

    // P1 and P2 are endpoints of seg
    let isP1InBox = ( (x1 > leftX) && (x1 < rightX) ) && ( (y1 > topY) && (y1 < bottomY) )
    let isP2InBox = ( (x2 > leftX) && (x2 < rightX) ) && ( (y2 > topY) && (y2 < bottomY) )

    let isAnyPointInBox = isP1InBox || isP2InBox

    let leftSide = {X = leftX; Y = topY}, {X = leftX; Y = bottomY}
    let rightSide = {X = rightX; Y = topY}, {X = rightX; Y = bottomY}
    let topSide = {X = leftX; Y = topY}, {X = rightX; Y = topY}
    let bottomSide = {X = leftX; Y = bottomY}, {X = rightX; Y = bottomY}

    let boxSidesCoords = [leftSide; rightSide; topSide; bottomSide]

    let findIntersections = 
        boxSidesCoords
        |> List.map (segmentIntersectsSegment (seg.Start, segEnd))
        |> List.tryFind (fun el -> el = true)

    if findIntersections = None && isAnyPointInBox = false then
        false
    else
        true

/// Function called when a wire has been clicked, so no need to be an option
let getClickedSegment (model:Model) (wireId: ConnectionId) (pos: XYPos) : SegmentId =
    let getIntersectingSegments (model:Model) (wireId:ConnectionId) (selectBox:BoundingBox) : ASeg array =     
        model.WX.[wireId].Segments
        |> Array.filter (fun seg -> segmentIntersectsBoundingBoxCoordinates seg selectBox)
    
    let boundingBox = {X = pos.X - 5.0; Y = pos.Y - 5.0; H = 10.0; W = 10.0}
    let intersectingSegments = getIntersectingSegments model wireId boundingBox


    //getIntersecting segments may not return anything at low resolutions as the mouse was not on any segment, but in range of the wire bbox
    //In this case just return the segment closest to mouse position
    //TODO - should it just do this anyway?
    (Array.head intersectingSegments).Id

let segXDelta seg =
    match seg.Dir with
    | Horizontal -> seg.Length
    | Vertical -> 0.

/// change the middle X coordinate of the joined ends of two segments (seg0 is LH, seg1 is RH).
/// compensate for negative signs in coordinates using as value but preserving sign
/// xPos is asumed positive
let moveXJoinPos xPos seg0 seg1 =
    let seg0End = segmentEndCoords seg0
    let dLen0 = xPos - seg0End.X

    [ {seg0 with Length = seg0.Length + dLen0}; {seg1 with Start = {seg1.Start with X = xPos}} ]

let changeLengths isAtEnd seg0 seg1 =
    let outerSeg, innerSeg =
        if isAtEnd then seg1, seg0 else seg0, seg1
    let innerX = segXDelta innerSeg
    let outerX = segXDelta outerSeg

    // should never happen, can't do anything
    if seg0.Dir <> Horizontal || seg1.Dir <> Horizontal || outerX < 0.0 then [seg0 ; seg1]
    elif innerX < 0.0 then  
        // the case where we need to shorten the first or last segment (seg0 here)
        moveXJoinPos (if isAtEnd then (segmentEndCoords seg1).X - Wire.stickLength else seg0.Start.X + Wire.stickLength) seg0 seg1
    else [ seg0; seg1]


/// This function allows a wire segment to be moved a given amount in a direction perpedicular to
/// its orientation (Horizontal or Vertical). Used to manually adjust routing by mouse drag.
/// The moved segment is tagged by negating one of its coordinates so that it cannot be auto-routed
/// after the move, thus keeping the moved position.
let moveSegment (seg:ASeg) (mousePos: XYPos) (model:Model) =
    let wire = model.WX.[seg.HostId]
    let index = segmentIndex seg model
    if index <= 0 || index >= wire.Segments.Length - 1 then
        failwithf $"Buswire segment index {index} out of range in moveSegment in wire length {wire.Segments.Length}"
    let prevSeg = wire.Segments.[index-1]
    let nextSeg = wire.Segments.[index+1]
    
    let inPortSide, outPortSide = Symbol.getTwoPortSides (model.Symbol) (wire.InputPort) (wire.OutputPort)
    let wireStartPos = wire.Segments[0].Start
    let wireEndPos = segmentEndCoords wire.Segments[6]
    let initialAutoWire = {wire with Segments = makeInitialSegmentsList wire.Id (wireEndPos, wireStartPos) (inPortSide, outPortSide)}
    let initialSeg = initialAutoWire.Segments[index]

    // adjust length of segments 0 and 6
    let hStickLen =
        if inPortSide <> outPortSide then
            min (abs  (wireStartPos.X - wireEndPos.X)/2.0) Wire.stickLength
        else
            Wire.stickLength
    let vStickLen =
        if inPortSide <> outPortSide then
            min (abs  (wireStartPos.Y - wireEndPos.Y)/2.0) Wire.stickLength
        else
            Wire.stickLength

    let isSafeMove =
        match seg.Dir with 
        | Horizontal ->
            // prevent user from moving horizontal segment too upwards or too downwards
            if (initialSeg.Start.Y > wireStartPos.Y && outPortSide = Symbol.Side.B) && (mousePos.Y < wireStartPos.Y + vStickLen) then
                false
            elif (initialSeg.Start.Y < wireStartPos.Y && outPortSide = Symbol.Side.T) && (mousePos.Y > wireStartPos.Y - vStickLen) then
                false
            elif (initialSeg.Start.Y > wireEndPos.Y && inPortSide = Symbol.Side.B) && (mousePos.Y < wireEndPos.Y + vStickLen) then
                false
            elif (initialSeg.Start.Y < wireEndPos.Y && inPortSide = Symbol.Side.T) && (mousePos.Y > wireEndPos.Y  - vStickLen)then
                false
            else 
                true
        | Vertical -> 
            // prevent user from moving vertical segment too leftwards or rightwards
            if (initialSeg.Start.X > wireStartPos.X && outPortSide = Symbol.Side.R) && (mousePos.X < wireStartPos.X + hStickLen) then
                false
            elif (initialSeg.Start.X < wireStartPos.X && outPortSide = Symbol.Side.L) && (mousePos.X > wireStartPos.X - hStickLen) then
                false
            elif (initialSeg.Start.X > wireEndPos.X && inPortSide = Symbol.Side.R) && (mousePos.X < wireEndPos.X + hStickLen) then
                false
            elif (initialSeg.Start.X < wireEndPos.X && inPortSide = Symbol.Side.L) && (mousePos.X > wireEndPos.X - hStickLen) then
                false
            else 
                true

    if seg.Dir = prevSeg.Dir || seg.Dir = nextSeg.Dir then
        wire
    else
        //runTestFable()
        let vPrevSeg = {prevSeg with Length = mousePos.X - prevSeg.Start.X}
        let vSeg = {seg with Start = {seg.Start with X = mousePos.X}}
        let vNextSeg = {nextSeg with Start = {nextSeg.Start with X = mousePos.X}; Length = (segmentEndCoords nextSeg).X - mousePos.X}
        let hPrevSeg = {prevSeg with Length = mousePos.Y - prevSeg.Start.Y}
        let hSeg = {seg with Start = {seg.Start with Y = mousePos.Y}}
        let hNextSeg = {nextSeg with Start = {nextSeg.Start with Y = mousePos.Y}; Length = (segmentEndCoords nextSeg).Y - mousePos.Y}
    
        let newPrevSeg, newSeg, newNextSeg = 
            match seg.Dir with
            | Vertical ->
                if isSafeMove then
                    vPrevSeg, vSeg, vNextSeg
                else
                    prevSeg, seg, nextSeg
            | Horizontal -> 
                if isSafeMove then
                    hPrevSeg, hSeg, hNextSeg
                else
                    prevSeg, seg, nextSeg

            // Add stickiness
            |> (
                fun (prev, s, next) ->
                      // if the previous segment becomes very short, make it 0 length and compensate current and next segments
                      if (index > 1) && (abs prev.Length > 0.) && (abs prev.Length < Wire.stickLength / 4.0) then
                        {prev with Length = 0.},
                        {s with Start = prev.Start},
                        {next with Start = segmentEndCoords {s with Start = prev.Start}; Length = next.Length + prev.Length}
                      // if the next segment becomes very short, make it 0 length and compensate previous and current segments
                      elif (index < 5) && (abs next.Length > 0.) && (abs next.Length < Wire.stickLength / 4.0) then
                        {prev with Length = prev.Length + next.Length},
                        {s with Start =  segmentEndCoords {prev with Length = prev.Length + next.Length}},
                        {next with Start = segmentEndCoords {s with Start =  segmentEndCoords {prev with Length = prev.Length + next.Length}}; Length = 0.}
                      else
                        prev, s, next
                )
        
        let newSegments =
            let wireSegs = wire.Segments |> Array.toList
            wireSegs.[.. index-2] @ [newPrevSeg; newSeg; newNextSeg] @ wireSegs.[index+2 ..]
            |> List.toArray

        {wire with Segments = newSegments}

/// Initialisatiton with no wires
let init () =
    let symbols,_ = Symbol.init()
    {   
        WX = Map.empty;
        FromVerticalToHorizontalSegmentIntersections = Map.empty;
        FromHorizontalToVerticalSegmentIntersections = Map.empty;
        Symbol = symbols; 
        CopiedWX = Map.empty; 
        SelectedSegment = SegmentId(""); 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
        Style = Classic
    } , Cmd.none

///Returns the wires connected to a list of components given by componentIds
let getConnectedWires (wModel : Model) (compIds : list<ComponentId>) =
    let inputPorts, outputPorts = Symbol.getPortLocations wModel.Symbol compIds
    wModel.WX
    |> Map.toList
    |> List.map snd
    |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts || Map.containsKey wire.OutputPort outputPorts)
    |> List.map (fun wire -> wire.Id)
    |> List.distinct

///Returns a tuple of: wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
let filterWiresByCompMoved (wModel : Model) (compIds : list<ComponentId>) =
    let inputPorts, outputPorts = Symbol.getPortLocations wModel.Symbol compIds

    let lst = 
        wModel.WX
        |> Map.toList
        |> List.map snd

    let inputWires =
        lst
        |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts)
        |> List.map (fun wire -> wire.Id)
        |> List.distinct

    let outputWires =
        lst
        |> List.filter (fun wire -> Map.containsKey wire.OutputPort outputPorts)
        |> List.map (fun wire -> wire.Id)
        |> List.distinct

    let fullyConnected =
        lst
        |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts && Map.containsKey wire.OutputPort outputPorts)
        |> List.map (fun wire -> wire.Id)
        |> List.distinct

    (inputWires, outputWires, fullyConnected)

/// Returns a newly autorouted wire given a model and wire
let autorouteWire (model : Model) (wire : Wire) : Wire =
    let posTuple = Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    let sideTuple = Symbol.getTwoPortSides (model.Symbol) (wire.InputPort) (wire.OutputPort)

    {wire with Segments = makeInitialSegmentsList wire.Id posTuple sideTuple}

/// reverse segment order, and Start, End coordinates, so list can be processed from input to output
/// this function is self-inverse
let revSegments (segs:ASeg array) =
    Array.rev segs
    |> Array.map (fun seg -> {seg with Start = (segmentEndCoords seg); Length = - seg.Length})

//
//  ====================================================================================================================
//
//                                        WIRE SEGMENTS FOR ROUTING
//
//
// Segments, going from Start (output port) to End (input port) coords, are summarised as:
// H => Horizontal (incr X)
// V => Vertical (incr Y)
// 0 => zero length segment (never used)
//
// segment qualifiers:
// F => min length (next to output or input, cannot be shortened)
//
// "Simple" case where output.X < input.X and 3 segment autoroute is possible
//  S0.FH  S1.0V  S2.H  S3.V  S4.H  S5.0V S6.FH
//
// "Complex" case where output.X > input.X and wire ends back for 5 segment autoroute
//  S0.FH  S1.V  S2.H  S3.V  S4.H  S5.0V S6.FH (not sure if H and V are correct here)
//
// To determine adjustment on End change we just reverse the segment and apply the Start change algorithm
// Adjustment => reverse list of segments, swap Start and End, and alter the sign of all coordinates
// For simplicity, due to the encoding of manual changes into coordinates by negating them (yuk!)
// we do not alter coordinate sign. Instead we invert all numeric comparisons.
// There are no constants used in the algorithm (if there were, they would need to be negated)
//
// ======================================================================================================================


let inline addPosPos (pos1: XYPos) (pos:XYPos) =
    {X = pos1.X + pos.X; Y = pos1.Y + pos.Y}

let inline moveEnd (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:ASeg) -> if i = n then {seg with Length = seg.Length + ((mover (segmentEndCoords seg)).X - (segmentEndCoords seg).X + (mover (segmentEndCoords seg)).Y - (segmentEndCoords seg).Y)} else seg)

let inline moveStart (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:ASeg) -> if i = n then {seg with Start = mover seg.Start} else seg)

let inline moveAll (mover: XYPos -> XYPos) (n : int) =
    List.mapi (fun i (seg:ASeg) -> if i = n then {seg with Start = mover seg.Start; Length = seg.Length + ((mover (segmentEndCoords seg)).X - (segmentEndCoords seg).X + (mover (segmentEndCoords seg)).Y - (segmentEndCoords seg).Y)} else seg)

let transformXY tX tY (pos: XYPos) =
    {pos with X = tX pos.X; Y = tY pos.Y}

let transformSeg tX tY (seg: ASeg) =
    let trans = transformXY tX tY
    let newStart = trans seg.Start
    let newEnd = trans (segmentEndCoords seg)
    {seg with Start = newStart; Length = newEnd.X - newStart.X + newEnd.Y - newStart.Y}

let topology (pos1: XYPos) (pos2:XYPos) =
    sign (abs pos1.X - abs pos2.X), sign (abs pos1.Y - abs pos2.Y)

/// Returns None if full autoroute is required or Some segments with initial part of the segment list autorouted
/// up till the first dragged (manually routed) segment.
/// ReverseFun must equal not or id. not => the segments go from input to output (reverse of normal).
/// This allows the same code to work on both ends of the wire, with segment reversal done outside this
/// function to implement input -> output direction.
let partialAutoRoute (segs: ASeg array) (newPortPos: XYPos) =
    let wirePos = segmentEndCoords segs.[0]
    let portPos = segs.[0].Start
    let newWirePos = {newPortPos with X = newPortPos.X + (abs wirePos.X - portPos.X) }
    let (diff:XYPos) = {X=newPortPos.X-portPos.X; Y= newPortPos.Y - portPos.Y}
    let lastAutoIndex =
        let isNegative (pos:XYPos) = pos.X < 0.0 || pos.Y < 0.0
        let isAutoSeg seg = (seg.ManualRoute = false)
        segs
        |> Array.takeWhile isAutoSeg
        |> Array.length
        |> (fun n -> if n > 5 then None else Some (n + 1))
    let scaleBeforeSegmentEnd segIndex =
        let seg = segs.[segIndex]
        let fixedPt = getAbsXY (segmentEndCoords seg)
        let scale x fx nx wx =
            if nx = fx then x else ((abs x - fx)*(nx-fx)/(abs wx - fx) + fx) * float (sign x)
        let startPos = if segIndex = 1 then portPos else wirePos
        let newStartPos = if segIndex = 1 then newPortPos else newWirePos
        let scaleX x = scale x fixedPt.X newStartPos.X startPos.X
        let scaleY y = scale y fixedPt.Y newStartPos.Y startPos.Y
        match List.splitAt (segIndex+1) (Array.toList segs), segIndex with
        | ((scaledSegs), otherSegs), 1 ->
            Some (((List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
                  |> List.toArray)
        | ((firstSeg :: scaledSegs), otherSegs), _ ->
            Some (((moveAll (addPosPos diff) 0 [firstSeg] @ List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
                  |> List.toArray)
        | _ -> None

    let checkTopology index =
        let finalPt = segs.[6].Start
        let oldTop x = topology (if index = 1 then portPos else wirePos) x
        let newTop x = topology (if index = 1 then newPortPos else newWirePos) x
        if oldTop finalPt <> newTop finalPt then
            // always aandon manual routing
            None 
        else
            let manSegEndPt = segmentEndCoords segs.[index]
            let oldT = oldTop manSegEndPt
            let newT = newTop manSegEndPt
            if oldT = newT then
                Some index
            else
                None
    lastAutoIndex
    |> Option.bind checkTopology
    |> Option.bind scaleBeforeSegmentEnd

///Moves a wire by a specified amount by adding a XYPos to each start and end point of each segment
let moveWire (wire : Wire) (diff : XYPos) =    
    let negXYPos (pos : XYPos) (diff : XYPos) : XYPos =
        let newPos = Symbol.posAdd (getAbsXY pos) diff
        if pos.X < 0. || pos.Y < 0. then {X = - newPos.X; Y = - newPos.Y}
        else newPos
    
    {wire with 
        Segments = 
            wire.Segments
            |> Array.map (fun seg ->
                         let newStart = negXYPos seg.Start diff
                         let newEnd = negXYPos (segmentEndCoords seg) diff
                         {seg with
                                Start = newStart
                                Length = newEnd.X - newStart.X + newEnd.Y - newStart.Y
                         })
    }

/// Re-routes a single wire in the model when its ports move.
/// Tries to preserve manual routing when this makes sense, otherwise re-routes with autoroute.
/// Partial routing from input end is done by reversing segments and and swapping Start/End
/// inout = true => reroute input (target) side of wire.
let updateWire (model : Model) (wire : Wire) (inOut : bool) =
    let newPort = 
        match inOut with
        | true -> Symbol.getInputPortLocation model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation model.Symbol wire.OutputPort
    if inOut then
        partialAutoRoute (revSegments wire.Segments) newPort
        |> Option.map revSegments
    else 
        partialAutoRoute wire.Segments newPort
    |> Option.map (fun segs -> {wire with Segments = segs})
    |> Option.defaultValue (autorouteWire model wire)


/// Updates the wire model by updating the JumpCoordinateList of all ASegs in a model, 
/// except for the ASegs belonging to wires found in the wiresToReset list.
let updateModelJumps (wiresToReset: ConnectionId list) (model: Model) =
    let wiresToResetA = List.toArray wiresToReset

    let allASegs =
        model.WX
        |> Map.toArray
        |> Array.map (fun (wid, w) -> w.Segments)
        |> Array.collect id
    let hSegsA = allASegs |> Array.filter (fun seg -> seg.Dir = Horizontal)
    let vSegsA = allASegs |> Array.filter (fun seg -> seg.Dir = Vertical)

    let mutable newWireMap = model.WX
    /// Updates the JumpCoordinateList of the segment of interest (specified by segIndex), 
    /// and then the model.WX accordingly.
    let updateWXJumps wireID segIndex jumps =
        let jumps = List.sortDescending jumps
        let updateSegJumps segs =
            segs
            |> Array.mapi (fun idx seg ->
                if idx <> segIndex then 
                    seg
                else 
                    { seg with JumpCoordinateList = jumps } )
        let updatedSegments = updateSegJumps newWireMap.[wireID].Segments
        newWireMap <- newWireMap |> Map.add wireID { newWireMap.[wireID] with Segments = updatedSegments } 

    let compareAndUpdateJumps h =
        let mutable jumps: (float * SegmentId) list = []
        let calculateJumpCoords v =
            let x, x1, x2 = v.Start.X, h.Start.X, (segmentEndCoords h).X
            let y, y1, y2 = h.Start.Y, v.Start.Y, (segmentEndCoords v).Y
            let xhi, xlo = max x1 x2, min x1 x2
            let yhi, ylo = max y1 y2, min y1 y2
            if x < xhi - 5.0 && x > xlo + 5.0 && y < yhi - 5.0 && y > ylo + 5.0 then
                jumps <- (x, v.Id) :: jumps
        if not (Array.contains h.HostId wiresToResetA) then
            vSegsA |> Array.map calculateJumpCoords |> ignore
        // compare jumps with what segment now has, and change WX if need be
        // note that if no change is needed we do not update WX
        // simple cases are done without sort for speed, proably not necessary!
        // The jump list is sorted in model to enable easier rendering of segments
        match jumps, h.JumpCoordinateList with
        | [], [] -> ()
        | [ a ], [ b ] when a <> b -> updateWXJumps h.HostId (segmentIndex h model) jumps
        | [], _ -> updateWXJumps h.HostId (segmentIndex h model) jumps
        | _, [] -> // in this case we need to sort the jump list
            updateWXJumps h.HostId (segmentIndex h model) (List.sort jumps)
        | newJumps, oldJ ->
            let newJ = List.sort newJumps
            // oldJ is already sorted (we only ever write newJ back to model)
            if newJ <> oldJ then updateWXJumps h.HostId (segmentIndex h model) newJumps else ()

    hSegsA |> Array.map compareAndUpdateJumps |> ignore

    { model with WX = newWireMap }

/// Updates the wire model by updating the JumpCoordinateList of all the ASegs in a model.
let updateAllJumps (wModel: Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    let model = updateModelJumps [] wModel
    TimeHelpers.instrumentTime "UpdateJumps" startT
    model

/// Updates the wire model by resetting the JumpCoordinateList from all segments in a model.
let resetAllJumps (wModel : Model) : Model =
    let toReset = wModel.WX |> Map.toList |> List.map fst
    updateModelJumps toReset wModel

/// Re-routes or translates the wires in the model based on a list of components that have been moved.
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =
    let (inputWires, outputWires, fullyConnected) = filterWiresByCompMoved model compIdList

    // Arrays are faster to check than lists
    let inputWiresA = List.toArray inputWires
    let outputWiresA = List.toArray outputWires
    let fullyConnectedA = List.toArray fullyConnected

    let updatedWires = 
        model.WX
        |> Map.toArray
        |> Array.map (fun (cId, wire) -> 
            // Translate wires that are connected to both inputs and outputs.
            if Array.contains cId fullyConnectedA then 
                (cId, updateWire model wire false)
            // Only route wires connected to only input or output ports that moved for efficiency.
            elif Array.contains cId inputWiresA then 
                (cId, updateWire model wire true)
            elif Array.contains cId outputWiresA then
                (cId, updateWire model wire false)
            else (cId, wire))
        |> Map.ofArray
    
    {model with WX = updatedWires}

/// Updates the wire model based on the msg.
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd

    | UpdateWires (componentIdList, diff) -> 
        updateWires model componentIdList diff, Cmd.none

    | UpdateStyle (newStyle: WireStyle) ->
        {model with Style = newStyle}, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let portOnePos, portTwoPos = Symbol.getTwoPortLocations model.Symbol inputId outputId
        let portOneSide, portTwoSide = Symbol.getTwoPortSides model.Symbol inputId outputId
        let wireWidthFromSymbol = WireWidth.Configured 1
        let wireId = ConnectionId(JSHelpers.uuid())
        let segmentList = makeInitialSegmentsList wireId (portOnePos, portTwoPos) (portOneSide, portTwoSide)
        let newWire = 
            {   Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = segmentList }
        let wireAddedMap = Map.add newWire.Id newWire model.WX
        let newModel = updateAllJumps {model with WX = wireAddedMap}
        newModel, Cmd.ofMsg BusWidths

    | BusWidths ->
        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ wire  =
                let width =
                    match connWidths.[wire.Id] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor = if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with Width = width ; Color = newColor} )

            let addSymbolWidthFolder (m: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports.[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = m.[symId]
                    match symbol.Compo.Type with
                    | SplitWire n ->
                        match inPort.PortNumber with 
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym m)
                    | MergeWires ->
                        match inPort.PortNumber with 
                        | Some 0 -> 
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 -> 
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> m

            let newWX = 
                (Map.empty, model.WX) ||> Map.fold addWireWidthFolder

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWX) ||> Map.fold addSymbolWidthFolder

            { model with 
                WX = newWX; 
                Notifications = None ; 
                ErrorWires=[]; 
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}}, Cmd.none    

        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model )
        
        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
            { model with Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)
    
    | CopyWires (connIds : list<ConnectionId>) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.WX
        { model with CopiedWX = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) -> 
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}
                    else if List.contains id model.ErrorWires then 
                        {wire with Color = HighLightColor.DarkSlateGrey}
                    else 
                        wire
                ) 
        {model with WX = newWX ; ErrorWires = connectionIds}, Cmd.none

    | SelectWires (connectionIds : list<ConnectionId>) -> // selects all wires in connectionIds, and also deselects all other wires
        // Arrays are faster to check than lists
        let connectionIdsA = List.toArray connectionIds
        let errorWiresA = List.toArray model.ErrorWires
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if Array.contains id errorWiresA then
                        if Array.contains id connectionIdsA then 
                            {wire with Color = HighLightColor.Brown} 
                        else 
                            {wire with Color = HighLightColor.Red}
                    else if Array.contains id connectionIdsA then
                        {wire with Color = HighLightColor.Purple} 
                    else
                        {wire with Color = HighLightColor.DarkSlateGrey} 
                ) 
        {model with WX = newWX}, Cmd.none

    | DeleteWires (connectionIds : list<ConnectionId>) -> 
        let newModel = resetAllJumps model
        // Arrays are faster to check than lists
        let connectionIdsA = List.toArray connectionIds
        let newWX =
            newModel.WX
            |> Map.filter (fun id wire -> not (Array.contains id connectionIdsA))
        {newModel with WX = newWX}, Cmd.ofMsg BusWidths

    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with
        | Down ->
            let segId = getClickedSegment model connId mMsg.Pos
            {model with SelectedSegment = segId }, Cmd.none

        | Drag ->
            let segId = model.SelectedSegment
            let getSelectedSeg (segArr: array<ASeg>) =
                let aSegOpt = segArr |> Array.tryFind (fun seg -> seg.Id = segId)
                match aSegOpt with
                | Some aSeg -> aSeg 
                | None -> failwithf "segment Id not found in segment list"
            let seg = getSelectedSeg (model.WX.[connId].Segments)

            if seg.Draggable then
                let newWire = moveSegment seg mMsg.Pos model
                let newWX = Map.add seg.HostId newWire model.WX
 
                {model with WX = newWX}, Cmd.none
            else
                model, Cmd.none

        | _ -> model, Cmd.none

    | ColorWires (connIds, color) -> // Just Changes the colour of the wires, Sheet calls pasteWires before this
        let updatedWires =
            (model.WX, connIds)
            ||> List.fold (fun prevWires cId -> 
                let oldWireOpt = Map.tryFind cId model.WX
                match oldWireOpt with
                | None -> 
                    printfn "BusWire error: expected wire in ColorWires does not exist"
                    prevWires
                | Some oldWire ->
                    Map.add cId { oldWire with Color = color } prevWires)
        { model with WX = updatedWires }, Cmd.none
    
    | ResetJumps ->
        let newModel = model |> resetAllJumps
        newModel, Cmd.none
    
    | MakeJumps ->
        let newModel = model |> updateAllJumps
        newModel, Cmd.none
    
    | ResetModel -> { model with WX = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none
    
    | LoadConnections conns -> // we assume components (and hence ports) are loaded before connections
        let posMatchesVertex (pos:XYPos) (vertex: float*float) =
            let epsilon = 0.00001
            let isGoodMatch = 
                abs (abs pos.X - abs (fst vertex)) < epsilon && abs (abs pos.Y - abs (snd vertex)) < epsilon
            if not isGoodMatch then 
                printf $"Bad wire endpoint match on {pos} {vertex}"; isGoodMatch
            else 
                isGoodMatch

        /// Get the connection ID and wire from given connection.
        let getConnIDandWire conn =
            let inputId = InputPortId conn.Target.Id
            let outputId = OutputPortId conn.Source.Id
            let connId = ConnectionId conn.Id
            let segments = issieVerticesToSegments connId conn.Vertices

            let wireFromConn = 
                { Id = ConnectionId conn.Id
                  InputPort = inputId
                  OutputPort = outputId
                  Color = HighLightColor.DarkSlateGrey
                  Width = 1
                  Segments = segments}

            let alignWireWithSymbol inOut (wire:Wire) =
                let getS (connId:string) = 
                    Map.tryFind connId model.Symbol.Ports
                    |> Option.map (fun port -> port.HostId)
                    |> Option.bind (fun symId -> Map.tryFind (ComponentId symId) model.Symbol.Symbols)
                    |> Option.map (fun sym -> sym.Compo.Label)
                printfn $"Updating loaded wire from {getS conn.Source.Id}->{getS conn.Target.Id} of wire "
                updateWire model wire inOut

            let updatedWire = 
                wireFromConn
                |> alignWireWithSymbol false
                |> alignWireWithSymbol true

            connId, updatedWire
        
        let newWX =
            conns
            |> List.toArray // Arrays are faster to check than lists
            |> Array.map getConnIDandWire
            |> Map.ofArray
        
        { model with WX = newWX }, Cmd.ofMsg (MakeJumps)

//---------------Other interface functions--------------------//

/// Used in Sheet.fs
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> = 
    let wireIntersectsBoundingBox (w : Wire) (bb : BoundingBox) =
        let boolArr = 
            w.Segments
            |> Array.map (fun seg -> segmentIntersectsBoundingBoxCoordinates seg bb) 
        Array.contains true boolArr
    
    wModel.WX
    |> Map.map (fun id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun id boolVal -> boolVal)
    |> Map.toList
    |> List.map (fun (id,bool) -> id)

/// Searches if the position of the cursor is on a wire in a model,
/// where n is 5 pixels, adjusted for top level zoom. Used in Sheet.fs.
let getWireIfClicked (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.X = pos.X - n; Y = pos.Y - n; H = n*2.; W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires

/// Interface function to paste symbols. 
/// It is a function instead of a message since output is required.
/// Used in Sheet.fs, called upon KeyPress CtrlV.
let pasteWires (wModel : Model) (newCompIds : list<ComponentId>) : (Model * list<ConnectionId>) =
    let oldCompIds = Symbol.getCopiedSymbols wModel.Symbol
    
    let pastedWires =
        let createNewWire (oldWire : Wire) =
            let newId = ConnectionId(JSHelpers.uuid())
            match Symbol.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->
                let portOnePos, portTwoPos = Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let portOneSide, portTwoSide = Symbol.getTwoPortSides wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let segmentList = makeInitialSegmentsList newId (portOnePos, portTwoPos) (portOneSide, portTwoSide)
                [|
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = segmentList;
                    }
                |]
            | None -> [||]
        
        wModel.CopiedWX
        |> Map.toArray // Arrays are faster to check than lists
        |> Array.map snd
        |> Array.collect createNewWire
        |> Array.map (fun wire -> wire.Id, wire)
        |> Map.ofArray
    
    let newWireMap = 
        (pastedWires, wModel.WX) 
        ||> Map.fold ( fun acc newKey newWire -> Map.add newKey newWire acc ) 

    let pastedConnIds =
        pastedWires
        |> Map.toList
        |> List.map fst
        
    { wModel with WX = newWireMap }, pastedConnIds

/// Used in Sheet.fs.
let getPortIdsOfWires (model: Model) (connIds: ConnectionId list) : (InputPortId list * OutputPortId list) =
    (([], []), connIds)
    ||> List.fold 
        (fun (inputPorts, outputPorts) connId -> 
            (model.WX.[connId].InputPort :: inputPorts, model.WX.[connId].OutputPort :: outputPorts))