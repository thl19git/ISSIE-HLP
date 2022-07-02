(*
This module draws schematics component symbols. Each symbol is associated with a unique Issie component.
*)

module Symbol
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
open System.Text.RegularExpressions


/// --------- STATIC VARIABLES --------- ///
let GridSize = 30 

/// --------- SYMBOL HELPER TYPES --------- ///
///Refers to the sides of a Symbol
type Side =
    | L
    | R
    | T
    | B

/// ---------- SYMBOL TYPES ---------- ///
/// Contains information related to visualisation of Components
type Symbol =
    {
        ///Midpoint of the symbol
        Pos: XYPos
        ///Input width option used by MergeWires etc.
        InWidth0: int option
        ///Input width option used by MergeWires etc.
        InWidth1: int option
        Id : ComponentId       
        Compo : Component                 
        Colour: string
        ShowInputPorts: bool
        ShowOutputPorts: bool
        Opacity: float
        Moving: bool
        ///True if the symbol is flipped (horizontal reflection)
        ///Maps port names to sides and indices. Port indices start at 0 for each side and begin at the bottom of left side (and rotate clockwise).
        TIPortMap: Map<string, Side * int>
        SelectedPort: Option<string>
    }

type Model = {
    Symbols: Map<ComponentId, Symbol>
    CopiedSymbols: Map<ComponentId, Symbol>
    Ports: Map<string, Port>                            // string since it's for both input and output ports

    InputPortsConnected:  Set<InputPortId>              // we can use a set since we only care if an input port 
                                                        // is connected or not (if so it is included) in the set 

    OutputPortsConnected: Map<OutputPortId, int>        // map of output port id to number of wires connected to that port
    }

//----------------------------Message Type-----------------------------------//


type Msg =
    | MouseMsg of MouseT
    | AddSymbol of pos:XYPos * compType:ComponentType * lbl: string
    | CopySymbols of ComponentId list
    | RotateSymbols of ComponentId list // Added message to rotate the symbol
    | FlipSymbols of ComponentId list // Added message to flip the symbol
    | ChangeSelectedPort of ComponentId list
    | MoveSelectedPort of ComponentId list
    | DeleteSymbols of sIds:ComponentId list
    | ShowAllInputPorts | ShowAllOutputPorts | DeleteAllPorts 
    | MoveSymbols of compList: ComponentId list * move: XYPos
    | ShowPorts of ComponentId list
    | SelectSymbols of ComponentId list// Issie interface
    | SymbolsHaveError of sIds: ComponentId list
    | ChangeLabel of sId : ComponentId * newLabel : string
    | PasteSymbols of sIds: ComponentId list
    | ColorSymbols of compList : ComponentId list * colour : HighLightColor
    | ErrorSymbols of errorIds: ComponentId list * selectIds: ComponentId list * isDragAndDrop: bool
    | ChangeNumberOfBits of compId:ComponentId * NewBits:int 
    | ChangeLsb of compId: ComponentId * NewBits:int64 
    | ChangeConstant of compId: ComponentId * NewBits:int64 * NewText:string
    | ResetModel // For Issie Integration
    | LoadComponents of  Component list // For Issie Integration
    | WriteMemoryLine of ComponentId * int64 * int64 // For Issie Integration 
    | WriteMemoryType of ComponentId * ComponentType

//---------------------------------helper types and functions----------------//

//---------------------------------------------------------------------------------//
//--------------------THL19 CODE SECTION STARTS-------------------------------------//
//---------------------------------------------------------------------------------//

///Finds the difference between two XYPos positions
let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

///Finds the sum of two XYPos positions
let posAdd (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

///Creates an XYPos given x and y coordinates
let posOf x y = {X=x;Y=y}


// ----- helper functions for titles ----- //

///Insert titles compatible with greater than 1 buswidth
let title t (n) =  
        if n = 1 then t else t + "(" + string(n-1) + "..0)"

///Insert titles for bus select
let bustitle wob lsb = 
    if wob <> 1 then"(" + string(wob + lsb - 1) + ".." + string(lsb) +  ")" else string(lsb)

///Decodes the component type into component labels
let prefix compType = 
    match compType with
    | Not | And | Or | Xor | Nand | Nor | Xnor -> "G"
    | Mux2 -> "MUX"
    | Demux2 -> "DM"
    | NbitsAdder _ -> "A"
    | NbitsXor _ -> "XOR"
    | DFF | DFFE -> "FF"
    | Register _ | RegisterE _ -> "REG"
    | AsyncROM1 _ -> "AROM"
    | ROM1 _ -> "ROM"
    | RAM1 _ -> "RAM"
    | AsyncRAM1 _ -> "ARAM"
    | Custom c ->
        c.Name + (if c.Name |> Seq.last |> System.Char.IsDigit then "." else "")
    | Constant1 _ -> "C"
    | BusCompare _ -> "EQ"
    | Decode4 -> "DEC"
    | BusSelection _ -> "SEL"
    | _ -> ""


//-----------------------------Skeleton Model Type for symbols----------------//

///Text to be put inside different Symbols depending on their ComponentType
let symbolLegendText (comp:Component) =
    match comp.Type with
    | And | Nand-> "&"
    | Or | Nor-> "≥1"
    | Xor | Xnor -> "=1"
    | Not -> "1"
    | Decode4 -> "Decode"
    | NbitsAdder n -> title "Adder" n
    | Register n | RegisterE n-> title "Register" n
    | AsyncROM1 _ -> "Async-ROM"
    | ROM1 _ -> "Sync-ROM"
    | RAM1 _ -> "Sync-RAM"
    | AsyncRAM1 _ -> "Async-RAM"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | NbitsXor (x)->   title "N-bits-Xor" x
    | Custom x -> x.Name
    | _ -> ""

///Input and Output names of the ports depending on their ComponentType
let portNameLists (comp:Component) = //(input port names, output port names)
    match comp.Type with
    | Decode4 -> (["Sel";"Data"],["0"; "1";"2"; "3"])
    | NbitsAdder _ -> (["Cin";"A";"B"],["Sum"; "Cout"])
    | Register _ -> (["D"],["Q"])
    | RegisterE _ -> (["D"; "EN"],["Q"])
    | ROM1 _ |AsyncROM1 _ -> (["Addr"],["Dout"])
    | RAM1 _ -> (["Addr"; "Din";"Wen" ],["Dout"])
    | AsyncRAM1 _ -> (["Addr"; "Din";"Wen" ],["Dout"])
    | DFF -> (["D"],["Q"])
    | DFFE -> (["D";"EN"],["Q"])
    | Mux2 -> (["0"; "1";"SEL"],["OUT"])
    | Demux2 -> (["IN" ; "SEL"],["0"; "1"])
    | NbitsXor _ -> (["P"; "Q"], ["Out"])
    | Custom x -> (List.map fst x.InputLabels), (List.map fst x.OutputLabels)
    |_ -> ([],[])
   // |Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
   // |Demux4 -> (["IN"; "SEL"],["0"; "1";"2"; "3";])
   // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
   // |Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
   // |_ -> ([],[])
   // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .

///Genererates a list of ports
let portLists numOfPorts hostID portType =
    if numOfPorts < 1 
    then []
    else
        [0..(numOfPorts-1)]
        |> List.collect (fun x ->
            [{
                Id = JSHelpers.uuid ()
                PortNumber = Some x
                PortType = portType
                HostId = hostID
            }])


//-----------------------Skeleton Message type for symbols---------------------//

///Rounds an integer to any given number. The first parameter is the number to round to, the second parameter is the input number that will be rounded
let roundToN (n : int) (x : int) =
    x + abs((x % n) - n)

///Finds the maximum label length in a list of ports
let customLabelsMaxLength (lst : (string * int) list) =
    let labelList = List.map (fst >> String.length) lst
    if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
    else List.max labelList

///Helper function to initialise each type of component
let makeComp (pos: XYPos) (comptype: ComponentType) (id:string) (label:string) (orientation:Rotation) (flipped:bool): Component =

    ///Function that helps avoid duplicate code by initialising parameters that are the same for all component types and takes as argument the others
    let makeComponent (n, nout, h, w) label : Component=  
        {
            Id = id 
            Type = comptype 
            Label = label 
            InputPorts = portLists n id PortType.Input 
            OutputPorts  = portLists nout id PortType.Output 
            X  = int (pos.X - float (w*GridSize) / 2.0) 
            Y = int (pos.Y - float (h*GridSize) / 2.0) 
            H = h * GridSize
            W = w * GridSize
            SymbolInfo = {Orientation = orientation;Flipped = flipped}
        }
    
    // match statement for each component type. the output is a 4-tuple that is used as an input to makecomponent (see below)
    // 4-tuple of the form ( number of input ports, number of output ports, Height, Width)
    let args = 
        match comptype with
        | ROM _ | RAM _ | AsyncROM _ -> 
            failwithf "What? Legacy RAM component types should never occur"
        | And | Nand | Or | Nor | Xnor | Xor ->  (2, 1, 2, 2) 
        | Not -> ( 1, 1, 2, 2) 
        | Input _ -> (0, 1, 1, 2)                
        | Output _ -> (1, 0, 1, 2) 
        | Viewer _ -> (1, 0, 1, 1) 
        | IOLabel -> (1, 1, 1, 2) 
        | Decode4 -> (2, 4, 4, 3) 
        | Constant1 (_,_,_) | Constant(_,_) -> (0, 1, 1, 2) 
        | MergeWires -> (2, 1, 2, 2) 
        | SplitWire _ -> (1, 2, 2, 2) 
        | Mux2 -> (3, 1, 3, 2) 
        // EXTENSION:    | Mux4 -> (5, 1, 5, 2)   
        // EXTENSION:    | Mux8 -> (9, 1, 7, 2) 
        | Demux2 -> (2, 2, 3, 2) 
        // EXTENSION:   | Demux4 -> (2, 4, 5, 2) 
        // EXTENSION:   | Demux8 -> (2, 8, 7, 2) 
        | BusSelection (_,_) -> (1, 1, 1, 2) 
        | BusCompare (_,_) -> (1, 1, 1, 2) 
        | DFF -> (1, 1, 3, 3) 
        | DFFE -> (2, 1, 3, 3) 
        | Register _ -> (1, 1, 3, 4 )
        | RegisterE _ -> (2, 1, 3, 4) 
        | AsyncROM1 _  -> (1, 1, 3, 4) 
        | ROM1 _ -> (1, 1, 3, 4) 
        | RAM1 _ | AsyncRAM1 _ -> (3, 1, 4, 4) 
        | NbitsXor _ -> (2, 1, 3, 4) 
        | NbitsAdder _ -> (3, 2, 3, 4) 
        | Custom x -> 
            let h = 1 + (List.max [List.length x.InputLabels; List.length x.OutputLabels])
            let maxInLength, maxOutLength = customLabelsMaxLength x.InputLabels, customLabelsMaxLength x.OutputLabels
            let maxW = maxInLength + maxOutLength + label.Length
            let scaledW = roundToN GridSize (maxW * GridSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
            let w = max (scaledW/GridSize) 4 //Ensures a minimum width if the labels are very small
            ( List.length x.InputLabels, List.length x.OutputLabels, h, w)
                
    makeComponent args label

///Helper function to initialise the port map for a given component type
let initialisePortMap (comptype: ComponentType) : Map<string, Side * int> =
    let portlists = 
        match comptype with
        | Decode4 -> [("Sel",(L,1));("Data",(L,0));("0",(R,0));("1",(R,1));("2",(R,2));("3",(R,3))]
        | NbitsAdder _ -> [("Cin",(T,0));("A",(L,1));("B",(L,0));("Sum",(R,0));("Cout",(B,0))]
        | Register _ -> [("D",(L,0));("Q",(R,0))]
        | RegisterE _ -> [("D",(L,1));("EN",(L,0));("Q",(R,0))]
        | ROM1 _ |AsyncROM1 _ -> [("Addr",(L,0));("Dout",(R,0))]
        | RAM1 _ -> [("Addr",(L,2));("Din",(L,1));("Wen",(L,0));("Dout",(R,0))]
        | AsyncRAM1 _ -> [("Addr",(L,2));("Din",(L,1));("Wen",(L,0));("Dout",(R,0))]
        | DFF -> [("D",(L,0));("Q",(R,0))]
        | DFFE -> [("D",(L,1));("EN",(L,0));("Q",(R,0))]
        | Mux2 -> [("0",(L,1));("1",(L,0));("SEL",(B,0));("OUT",(R,0))]
        | Demux2 -> [("IN",(L,0));("SEL",(B,0));("0",(R,1));("1",(R,0))]
        | NbitsXor _ -> [("P",(L,1));("Q",(L,0));("Out",(R,0))]
        | Custom x -> let inputs = List.map fst x.InputLabels
                      let outputs = List.map fst x.OutputLabels
                      let inputTuples = inputs |> List.mapi (fun i p -> (p,(L,inputs.Length-1-i)))
                      let outputTuples = outputs |> List.mapi (fun i p -> (p,(R,i)))
                      inputTuples @ outputTuples
        | _ -> []
    portlists |> Map.ofList

///Returns the initial port that should be selected, given a component
let portSelectInitial (comp: Component)=
    match comp.Type with
    | Decode4 -> Some "Sel"
    | NbitsAdder _ -> Some "A"
    | Register _ -> Some "D"
    | RegisterE _ -> Some "D"
    | ROM1 _ |AsyncROM1 _ -> Some "Addr"
    | RAM1 _ -> Some "Addr"
    | AsyncRAM1 _ -> Some "Addr"
    | DFF -> Some "D"
    | DFFE -> Some "D"
    | Mux2 -> Some "0"
    | Demux2 -> Some "IN"
    | NbitsXor _ -> Some "P"
    | Custom x -> let inputs = List.map fst x.InputLabels
                  Some inputs.Head
    | _ -> None

///Applies a rotation to a side
let rotateSide (side:Side) (rotation:Rotation)=
    match side, rotation with
    | L, R0 | B, R90 | R, R180 | T, R270 -> L
    | T, R0 | L, R90 | B, R180 | R, R270 -> T
    | R, R0 | T, R90 | L, R180 | B, R270 -> R
    | B, R0 | R, R90 | T, R180 | L, R270 -> B

///Given a symbol and a port name, returns a new port map with that port moved anticlockwise
let movePort (symb: Symbol) (selPortName: string) : Map<string,Side*int> =
    let getAdjacentPorts (side:Side) = (Map.filter (fun (key:string) (v:(Side*int)) -> (key <> selPortName && (fst v =side))) symb.TIPortMap )
    let decrementPortIdxs (side:Side) (portMap:Map<string,Side*int>) = portMap |> Map.map (fun (key:string) (v:Side*int) -> (fst v,(snd v) - if(v |> fst = side) then 1 else 0))

    let selPortSide,selPortIdx = Map.find selPortName symb.TIPortMap
    let oldAdjacentPorts = selPortSide |> getAdjacentPorts |> Map.toList |> List.sortBy(fun (elem:string*(Side*int)) -> -(snd (snd elem)))
    let newSide = if selPortIdx = 0 then rotateSide selPortSide R270 else selPortSide
    let newAdjacentPorts = newSide |> getAdjacentPorts |> Map.toList |> List.sortBy(fun (elem:string*(Side*int)) -> -(snd (snd elem)))
    let newIdx =
        if selPortIdx=0 then
            let maxIdx = if (List.isEmpty newAdjacentPorts) then -1 else List.head newAdjacentPorts |> snd |> snd
            maxIdx+1
        else
            selPortIdx-1
    let swappedPortName = if selPortIdx=0 then None else List.pick (fun (elem:(string*(Side*int))) -> if (snd (snd elem) = selPortIdx-1) then Some (Some (fst elem)) else None) newAdjacentPorts
    let portMap = Map.change selPortName (fun x-> Some(newSide,newIdx)) symb.TIPortMap
    if swappedPortName <> None then
        Map.change swappedPortName.Value (fun x -> Some(selPortSide,selPortIdx)) portMap
    else
        portMap |> decrementPortIdxs selPortSide

///Given a symbol, changes the selected port
///If the symbol is not flipped this is the next port going clockwise
///Else it is the next port going anticlockwise
let changePortSelection (symbol: Symbol)=
    match symbol.SelectedPort with
    | None -> portSelectInitial symbol.Compo
    | Some selectedPort ->
        let portMap = symbol.TIPortMap
        let side,index = portMap[selectedPort]
        let portsOnSide sd=
            portMap
            |> Map.toList
            |> List.filter (fun (_, (s,_)) -> s = sd)
            |> List.length
        let nextSide =
            [(side,R90);(side,R180);(side,R270)]
            |> List.map (fun (s,r) -> rotateSide s r)
            |> List.map (fun s -> (s, portsOnSide s))
            |> List.filter (fun (_,num) -> num > 0)
            |> List.map fst
        let portList = 
            if index = (portsOnSide side) - 1 then
                if nextSide.IsEmpty then
                    portMap 
                    |> Map.toList
                    |> List.filter (fun (_, (_,i)) -> i = 0)
                else
                    portMap 
                    |> Map.toList
                    |> List.filter (fun (_, (s,i)) -> s = nextSide.Head && i = 0)
            else
                portMap 
                |> Map.toList
                |> List.filter (fun (_, (s,i)) -> s = side && i = index + 1)
        portList
        |> List.head
        |> fst
        |> Some


///Given a Symbol and a point (XYPos), finds the transformed coordinates given the orientation and flip of the symbol
let flipAndRotatePoint (comp: Component) (point: XYPos)=
    let halfH = float(comp.H/2)
    let halfW = float(comp.W/2)
    let (cosTheta, sinTheta) =
        match comp.SymbolInfo.Orientation with
        | R0 -> 1., 0.
        | R90 -> 0., 1.
        | R180 -> -1., 0.
        | R270 -> 0., -1.
    let shifted = {X= point.X-halfW; Y= point.Y-halfH} //Shifts the midpoint to the local origin
    let flipped = if comp.SymbolInfo.Flipped then {X= shifted.X; Y= -1.*shifted.Y} else {X= shifted.X; Y= shifted.Y}
    let rotated = {X= flipped.X*cosTheta - flipped.Y*sinTheta; Y= flipped.X*sinTheta + flipped.Y*cosTheta}
    {X= rotated.X+halfW; Y= rotated.Y+halfH} //Shifts the local origin back to the midpoint

///Converts a list of XYPos positions into a string compatible with drawing helpers
let XYPositionsToString (positions: XYPos list)=
    let positionsList = positions |> List.map (fun p -> string p.X + "," + string p.Y + " ")
    String.concat "" positionsList
   
///Generate a new symbol of a given ComponentType
let createNewSymbol (pos: XYPos) (comptype: ComponentType) (label:string) =
    let id = JSHelpers.uuid ()
    let comp = makeComp pos comptype id label R0 false
    { 
      Pos = { X = pos.X - float comp.W / 2.0; Y = pos.Y - float comp.H / 2.0 }
      ShowInputPorts = false
      ShowOutputPorts = false
      InWidth0 = None // set by BusWire
      InWidth1 = None
      Colour = "lightgrey"
      Id = ComponentId id
      Compo = comp
      Opacity = 1.0
      Moving = false
      TIPortMap = initialisePortMap comptype
      SelectedPort = None
    }

///Adds ports to port model     
let addToPortModel (model: Model) (comp: Component) =
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) =
        Map.add port.Id port currentPorts
    
    let addedInputPorts = (model.Ports, comp.InputPorts) ||> List.fold addOnePort
    (addedInputPorts, comp.OutputPorts) ||> List.fold addOnePort

//-----------------------------------------GET PORT POSITION---------------------------------------------------
// Function that calculates the positions of the ports 

///Hack so that bounding box of splitwire, mergewires can be smaller height relative to ports
let inline getPortPosEdgeGap (ct: ComponentType) =
    match ct with
    | MergeWires | SplitWire _  -> 0.25
    | _ -> 1.0

///Given a rotated side and a symbol rotation, finds the unrotated side
let unrotateSide (side:Side) (rotation:Rotation)=
    match side, rotation with
    | L, R0 | T, R90 | R, R180 | B, R270 -> L
    | T, R0 | R, R90 | B, R180 | L, R270 -> T
    | R, R0 | B, R90 | L, R180 | T, R270 -> R
    | B, R0 | L, R90 | T, R180 | R, R270 -> B

///Applies flipping to a side (L,R remain unchanged)
let flipSide (side:Side) (flipped:bool)=
    match side, flipped with
    | L, _ -> L
    | R, _ -> R
    | T, false | B, true -> T
    | B, false | T, true -> B

///Given a component and a side, finds the true side after flips and rotation
let trueSide (comp:Component) (side:Side)=
    rotateSide (flipSide side comp.SymbolInfo.Flipped) comp.SymbolInfo.Orientation

///Given a symbol and a transformed side, finds the side before flips and rotation
let inverseTransformSide (comp: Component) (side:Side)=
    unrotateSide (flipSide side comp.SymbolInfo.Flipped) comp.SymbolInfo.Orientation

///Counts the number of named ports on a particular side of a symbol
let countNamedPortsOnSide (symbol:Symbol) (side:Side)=
    let ports = symbol.TIPortMap
                |> Map.toList
                |> List.map snd
                |> List.map fst
                |> List.filter(fun x -> x = side)
    ports.Length

///Helper to determine whether or not a symbol has named ports
let hasNamedPorts (symbol:Symbol)=
    not (Map.isEmpty symbol.TIPortMap)

///Returns a list of untransformed sides without named ports
let portlessSides (symbol:Symbol) =
    match hasNamedPorts symbol with
    | false -> [T;B]
    | true  -> let portlist = Map.toList symbol.TIPortMap
               let rightPorts = ((List.filter (fun (_, (side,_)) -> side = R) portlist),R)
               let leftPorts = ((List.filter (fun (_, (side,_)) -> side = L) portlist),L)
               let topPorts = ((List.filter (fun (_, (side,_)) -> side = T) portlist),T)
               let bottomPorts = ((List.filter (fun (_, (side,_)) -> side = B) portlist),B)
               [topPorts;bottomPorts;rightPorts;leftPorts]
               |> List.filter (fun (l,_) -> l.IsEmpty)
               |> List.map (fun (_,s) -> s)

///Finds the coordinates of a port (given by side and index) of a symbol
let getPortPosCoords (comp: Component) (side: Side) (index: float) (numPorts: int)=
    let gap = getPortPosEdgeGap comp.Type
    let offset = // Offset handles sloped sides of Mux and Demux
        match comp.Type, side with
        | Mux2, T | Demux2, B -> 0.2*float(comp.H)*(1.+index)/(float(numPorts)+1.)
        | Mux2, B | Demux2, T -> 0.2*(float(comp.H)*(float(numPorts)-index)/(float(numPorts)+1.))
        | _, _ -> 0.
    let (posX, posY)=
        match side with
        | L -> (0.0, float(comp.H) - ((float(comp.H))*(( index + gap )/( float( numPorts ) + 2.0*gap - 1.0))))
        | R -> (float(comp.W), (float(comp.H))*(( index + gap )/( float( numPorts ) + 2.0*gap - 1.0)))
        | T -> ((float(comp.W))*(( index + gap )/( float( numPorts ) + 2.0*gap - 1.0)) ,offset)
        | B -> (float(comp.W) - ((float(comp.W))*(( index + gap )/( float( numPorts ) + 2.0*gap - 1.0))), float(comp.H)-offset)
    flipAndRotatePoint comp {X=posX; Y=posY}

///Finds the position of a port using the port name
let getPortPosByName (symbol: Symbol) (name:string)=
    let (side, index) = symbol.TIPortMap |> Map.find name
    let numPorts = countNamedPortsOnSide symbol side
    getPortPosCoords symbol.Compo side (float index) numPorts

///Finds the position of a port using the port
let getPortPos (symbol: Symbol) (port:Port) =
    let comp = symbol.Compo
    let (ports, fstOrSnd, side) =
        if port.PortType = (PortType.Input) then
            (comp.InputPorts, fst, L)
        else 
            (comp.OutputPorts, snd, R)
    let listIndex = List.findIndex (fun (p:Port)  -> p = port) ports 
    match hasNamedPorts symbol with         
    | true ->  match List.tryItem listIndex (fstOrSnd(portNameLists comp)) with
               | Some name -> getPortPosByName symbol name
               | _ -> failwithf "No name at list index"
    | false -> getPortPosCoords comp side (float listIndex) ports.Length

///Finds the side of a port given the port and the symbol
let getPortSide (symbol: Symbol) (port:Port) =
    let comp = symbol.Compo
    let (ports, fstOrSnd, side) =
        if port.PortType = (PortType.Input) then
            (comp.InputPorts, fst, L)
        else 
            (comp.OutputPorts, snd, R)
    let listIndex = List.findIndex (fun (p:Port)  -> p = port) ports 
    match hasNamedPorts symbol with         
    | true ->  match List.tryItem listIndex (fstOrSnd(portNameLists comp)) with
               | Some name ->
                    let (side, _) = symbol.TIPortMap |> Map.find name
                    trueSide comp side
               | _ -> failwithf "No name at list index"
    | false -> trueSide comp side
    
///Finds the position of a port given the port and model
let getPortPosModel (model: Model) (port:Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols) port

///Finds the side of a port given the port and model
let getPortSideModel (model: Model) (port:Port) =
    getPortSide (Map.find (ComponentId port.HostId) model.Symbols) port

//-----------------------------------------DRAWING HELPERS ---------------------------------------------------
///Text adding function with many parameters (such as bold, position and text)
let private addText posX posY name txtPos weight size=
    let text =
            {defaultText with TextAnchor = txtPos; FontWeight = weight; FontSize = size}
    [makeText posX posY name text]


///Adds port text given coordinates, name and side
let private portText symbol (pos:XYPos) name side =
    let weight =
        match symbol.SelectedPort with
        | Some n -> if n = name then "bold" else "normal"
        | None -> "normal"
    let (xPos,yPos,textAlign) =
        match side with
        | L -> (pos.X+5.,pos.Y-7.,"start")
        | R -> (pos.X-5.,pos.Y-7.,"end")
        | T -> (pos.X,pos.Y+2.,"middle")
        | B -> (pos.X,pos.Y-14.,"middle")
    (addText xPos yPos name textAlign weight "12px")

///Draws all port text for a given symbol
let private drawPortsText (symbol:Symbol) =
    let empty = symbol.TIPortMap |> Map.isEmpty
    if empty
    then []
    else
        symbol.TIPortMap
        |> Map.toList
        |> List.map (fun (name,(side,_)) -> portText symbol {X=(getPortPosByName symbol name).X;Y=(getPortPosByName symbol name).Y} name (trueSide symbol.Compo side))
        |> List.collect id


///Draws the ports of a symbol
let private drawPorts (symbol:Symbol) (printPorts:bool)=
    let portList = symbol.Compo.InputPorts @ symbol.Compo.OutputPorts
    if (portList.Length) < 1 || (not printPorts)
    then []
    else
        [0..(portList.Length-1)] |> List.collect (fun x -> [makeCircle (getPortPos symbol portList[x]).X (getPortPos symbol portList[x]).Y portCircle])
        

//------------------------------HELPER FUNCTIONS FOR DRAWING SYMBOLS-------------------------------------
///Creates a polygon from a string of points
let private createPolygon points colour opacity = 
    [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity}]

///Creates a bicolour polygon from a string of points
let createBiColourPolygon points colour strokeColour opacity strokeWidth= 
    if strokeColour <> "black" then 
        [makePolygon points {defaultPolygon with Fill = colour; Stroke = strokeColour; FillOpacity = opacity; StrokeWidth=strokeWidth}]
    else   
        [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity; StrokeWidth = strokeWidth}]

///Adds an invertor to a symbol at a given position
let addInvertor (comp:Component) (pos:XYPos) colour opacity =
    let pointsString =
        [(pos.X,pos.Y);(float pos.X+9.,pos.Y);(pos.X,pos.Y-8.)]
        |> List.map (fun (x,y) -> posOf x y)
        |> List.map (flipAndRotatePoint comp)
        |> XYPositionsToString
    createPolygon pointsString colour opacity

///Adds a clock indicator to a symbol
let addClock comp colour opacity =
    let h = comp.H
    let w = comp.W
    let (posX,posY) =
        match comp.SymbolInfo.Orientation with
        | R0  | R180 -> (0,comp.H)
        | R90 -> (w/2-h/2,h/2+w/2)
        | R270 -> match comp.Type with
                  | RAM1 _ | AsyncRAM1 _ -> (w/2-h/2,h/2-w/2+15)
                  | _ -> (w/2-h/2,h/2+w/2)
        
    let points = $"%i{posX},%i{posY-1} %i{posX+8},%i{posY-7} %i{posX},%i{posY-13}"
    createPolygon points colour opacity
    |> List.append (addText (float(posX+10)) (float(posY-13)) " clk" "start" "normal" "12px")

///Draws a light gray line given two XYPos endpoints
let addLine (pos1: XYPos) (pos2: XYPos) opacity = // 
    let points = $"%f{pos1.X},%f{pos1.Y} %f{pos2.X},%f{pos2.Y}"
    createPolygon points "lightgray" opacity

///Helper to determine outline colours
let outlineColour (colour:string) =
    match colour.ToLower() with
    | "lightgray" | "lightgrey" -> "black"
    | c -> 
        printfn $"color={colour}"
        c

///Adds the title for a symbol
let addTitle (symbol:Symbol) =
    let h = float symbol.Compo.H
    let w = float symbol.Compo.W
    let freeSides = portlessSides symbol
    let originalSide = inverseTransformSide symbol.Compo T
    let freeTopSide =
        freeSides
        |> List.map (trueSide symbol.Compo)
        |> List.filter (fun s -> s = T)
        |> List.isEmpty
        |> not
    if freeSides.IsEmpty then
        match symbol.Compo.SymbolInfo.Orientation with
        | R0  | R180 -> (addText 0. -20.0 symbol.Compo.Label "middle" "normal" "16px")
        | R90 | R270 -> (addText (w/2. - h/2.) (h/2. - w/2. - 20.0) symbol.Compo.Label "middle" "normal" "16px")
    else
        let (textPos,side) =
            if freeTopSide then
                match originalSide with
                | T -> {X= w*0.5; Y= 0.}, T
                | B -> {X= w*0.5; Y= h}, T
                | L -> {X= 0.; Y= h*0.5}, T
                | R -> {X= w; Y= h*0.5}, T
            else
                match freeSides.Head with
                | T -> {X= w*0.5; Y= 0.}, trueSide symbol.Compo T
                | B -> {X= w*0.5; Y= h}, trueSide symbol.Compo B
                | L -> {X= 0.; Y= h*0.5}, trueSide symbol.Compo L
                | R -> {X= w; Y= h*0.5}, trueSide symbol.Compo R
        let trueTextPos = flipAndRotatePoint symbol.Compo textPos
        match side with
        | T -> (addText (trueTextPos.X) (trueTextPos.Y-20.0) symbol.Compo.Label "middle" "normal" "16px")
        | B -> (addText (trueTextPos.X) (trueTextPos.Y+4.0) symbol.Compo.Label "middle" "normal" "16px")
        | L -> (addText (trueTextPos.X-10.0) (trueTextPos.Y-8.) symbol.Compo.Label "end" "normal" "16px")
        | R -> (addText (trueTextPos.X+10.) (trueTextPos.Y-8.) symbol.Compo.Label "start" "normal" "16px")

///Adds additional legend text inside of a symbol
let addSymbolLegendText (symbol:Symbol) =
    let h =
        match symbol.Compo.SymbolInfo.Orientation with
        | R0  | R180 -> float symbol.Compo.H
        | R90 | R270 -> float symbol.Compo.W
    let freeTopSide =
        portlessSides symbol
        |> List.map (trueSide symbol.Compo)
        |> List.filter (fun s -> s = T)
        |> List.isEmpty
        |> not
    let yOffset =
        match freeTopSide with
        | true  -> 5.0
        | false -> 0.2 * h 
    (addText ((float symbol.Compo.W)*0.5) (yOffset+(float symbol.Compo.H)*0.5-h*0.5) (symbolLegendText symbol.Compo) "middle" "bold" "14px") 

///Draws a coloured line given two (XYPos) endpoints and a desired colour
let addColourLine (pos1: XYPos) (pos2: XYPos) opacity (colour:string)= // TODO: Line instead of polygon?
    let points = $"%f{pos1.X},%f{pos1.Y} %f{pos2.X},%f{pos2.Y}"
    let olColour = outlineColour colour
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=olColour; StrokeWidth = "2.0"; FillOpacity = opacity}]

///Finds the points of a symbol outline, and returns in string form
let symbolPoints comp =
    let h = float comp.H
    let w = float comp.W
    let halfW = w/2.
    let halfH = h/2.
    let points =            // Points that specify each symbol 
        match comp.Type with
        | Input _ -> [(0.,0.);(0.,h);(w*0.8,h);(w,halfH);(w*0.8,0.)]
        | Constant1 _ -> [(0.,h);(halfW,halfH);(0.,0.)]
        | IOLabel -> [(w*0.33,0.);(0.,halfH);(w*0.33,h);(w*0.66,h);(w,halfH);(w*0.66,0.)]
        | Output _ -> [(w*0.2,0.);(0.,halfH);(w*0.2,h);(w,h);(w,0.)]
        | Viewer _ -> [(w*0.2,0.);(0.,halfH);(w*0.2,h);(w,h);(w,0.)]
        | MergeWires -> [(halfW,(1.0/6.0)*h);(halfW,(5.0/6.0)*h)]
        | SplitWire _ -> [(halfW,(1.0/6.0)*h);(halfW,(5.0/6.0)*h)]
        | Demux2 -> [(0.,h*0.2);(0.,h*0.8);(w,h);(w,0.)]
        | Mux2 -> [(0.,0.);(w,h*0.2);(w,h*0.8);(0.,h)]
        // EXTENSION: |Mux4|Mux8 -> [(0.,0.);(w,h*0.2);(w,h*0.8);(0.,h)]
        // EXTENSION: | Demux4 |Demux8 -> [(0.,h*0.2);(0.,h*0.8);(w,h);(w,0.)]
        | BusSelection _ |BusCompare _ -> [(0.,0.);(0.,h);(0.6*w,h);(0.8*w,0.7*h);(w,0.7*h);(w,0.3*h);(0.8*w,0.3*h);(0.6*w,0.)]
        | _ -> [(0.,h);(w,h);(w,0.);(0.,0.)]
    
    points
    |> List.map (fun (x,y) -> posOf x y)
    |> List.map (flipAndRotatePoint comp)
    |> XYPositionsToString

///Add extra text (bit indicators) to IO and bus related components
let additionalSymbolText (comp:Component)=
    let h = float comp.H
    let w = float comp.W
    let offset =
        match comp.SymbolInfo.Orientation with
        | R0  | R180 -> 0.
        | R90 | R270 -> 1.
    match comp.Type with
    | BusSelection(x,y) -> (addText (w/2.) (h/2.7-2.) (bustitle x y) "middle" "normal" "12px" )
    | BusCompare (_,y) -> (addText (w/2.) (h/2.7-1.) ("=" + NumberHelpers.hex(int y)) "middle" "bold" "10px")
    | Input (x) -> (addText  (w/2.) (h/2.7-3.) (title "" x) "middle" "normal" "12px")
    | Output (x) -> (addText  (w/2.) (h/2.7-3.0) (title "" x) "middle" "normal" "12px")
    | Viewer (x) -> (addText  (w/2.) (h/2.7-1.25) (title "" x) "middle" "normal" "9px")
    | Constant1 (_,_,txt) -> (addText (w/2.+8.*offset) (h/2.-5.*offset) txt "middle" "normal" "12px")
    | _ -> []

///Adds all extra elements to a symbol, such as lines, clocks and invertors
let symbolAdditions (symbol:Symbol) (colour:string) (opacity:float)=
    let comp = symbol.Compo
    let h = comp.H
    let w = comp.W
    let halfW = comp.W/2
    let halfH = (comp.H)/2

    let mergeSplitLine posX1 posX2 posY msb lsb =
        let text = 
            match msb = lsb, msb >= lsb with
            | _, false -> ""
            | true, _ -> $"({msb})"
            | false, _ -> $"({msb}:{lsb})"
        let p1 = flipAndRotatePoint comp {X=float posX1;Y=posY*(float h)}
        let p2 = flipAndRotatePoint comp {X=float posX2;Y=posY*(float h)}
        let textPos = flipAndRotatePoint comp {X=(float (posX1 + posX2)/2.0);Y=(posY*float(h)-11.0)}
        addColourLine p1 p2 opacity colour @
        addText textPos.X textPos.Y text "middle" "bold" "9px"

    // Offset allows use of code for both SplitWire and MergeWires
    let allMergeSplitLines offset midt midb msb =
        mergeSplitLine (0+offset) (halfW+offset) (1.0/6.0) midt 0 @ 
        mergeSplitLine (0+offset) (halfW+offset) (5.0/6.0) msb midb @ 
        mergeSplitLine (halfW-offset) (w-offset) 0.5 msb 0

    match comp.Type with
    | Constant1 (_,_,txt) ->
        let p1 = flipAndRotatePoint comp {X=float halfW; Y=float halfH}
        let p2 = flipAndRotatePoint comp {X=float w; Y=float halfH}
        (addLine p1 p2 opacity) 
    | Nand | Nor | Xnor |Not -> (addInvertor comp {X=float w; Y=float halfH} colour opacity)
    | MergeWires -> 
        let lo, hi = 
            match symbol.InWidth0, symbol.InWidth1  with 
            | Some n, Some m  -> n, m
            | _ -> -1,-1
        let msb = hi + lo - 1
        let midb = lo
        let midt = lo - 1
        allMergeSplitLines 0 midt midb msb
    | SplitWire mid -> 
        let msb, mid' =
            match symbol.InWidth0 with
            | Some n -> n - 1, mid
            | _ -> -100, -50
        let midb = mid'
        let midt = mid'-1
        allMergeSplitLines halfW midt midb msb
    | DFF |DFFE -> (addClock comp colour opacity)
    | Register _ |RegisterE _ -> (addClock comp colour opacity)
    | ROM1 _ |RAM1 _ | AsyncRAM1 _ -> (addClock comp colour opacity)
    | _ -> []

    

/// --------------------------------------- SYMBOL DRAWING ------------------------------------------------------ ///   
///Draws the symbol for a given component
let compSymbol (symbol:Symbol) (comp:Component) (colour:string) (showInputPorts:bool) (showOutputPorts:bool) (opacity: float)= 
    let olColour, strokeWidth =
        match comp.Type with
        | SplitWire _ | MergeWires -> outlineColour colour, "2.0"
        | _ -> "black", "1.0"
    (drawPorts symbol showOutputPorts)
    |> List.append (drawPortsText symbol)
    |> List.append (addSymbolLegendText symbol)
    |> List.append (addTitle symbol)
    |> List.append (symbolAdditions symbol colour opacity)
    |> List.append (additionalSymbolText comp)
    |> List.append (createBiColourPolygon (symbolPoints comp) colour olColour opacity strokeWidth)

let init () = 
    { Symbols = Map.empty; CopiedSymbols = Map.empty; Ports = Map.empty ; InputPortsConnected= Set.empty ; OutputPortsConnected = Map.empty}, Cmd.none

//----------------------------View Function for Symbols----------------------------//
type private RenderSymbolProps =
    {
        Symbol : Symbol 
        Dispatch : Dispatch<Msg>
        key: string 
    }

///View for one symbol. Using FunctionComponent.Of to improve efficiency (not printing all symbols but only those that are changing)
let private renderSymbol =
    
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let symbol = props.Symbol
            let ({X=fX; Y=fY}:XYPos) = symbol.Pos
            g ([ Style [ Transform($"translate(%f{fX}px, %f{fY}px)") ] ]) (compSymbol props.Symbol props.Symbol.Compo symbol.Colour symbol.ShowInputPorts symbol.ShowOutputPorts symbol.Opacity)
            
        , "Symbol"
        , equalsButFunctions
        )
    
///View function for symbol layer of SVG
let MapsIntoLists map =
    let listMoving = 
        Map.filter (fun _ sym -> not sym.Moving) map
        |>Map.toList
        |>List.map snd
    let listNotMoving =
        Map.filter (fun _ sym -> sym.Moving) map
        |>Map.toList
        |>List.map snd
    listMoving @ listNotMoving


let view (model : Model) (dispatch : Msg -> unit) = 
    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> MapsIntoLists
    |> List.map (fun ({Id = ComponentId id} as symbol) ->
        renderSymbol
            {
                Symbol = symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList
    |> TimeHelpers.instrumentInterval "SymbolView" start

//---------------------------------------------------------------------------------//
//--------------------THL19 CODE SECTION ENDS--------------------------------------//
//---------------------------------------------------------------------------------//

//------------------------GET BOUNDING BOXES FUNCS--------------------------------used by sheet.
// Function that returns the bounding box of a symbol. It is defined by the height and the width as well as the x,y position of the symbol
let getBoundingBoxofSymbol (sym:Symbol): BoundingBox =
    let delta = (sym.Compo.W - sym.Compo.H)/2
    let offset =
        match sym.Compo.W > sym.Compo.H with
        | true -> (delta, delta)
        | false -> (-delta, -delta)

    match sym.Compo.SymbolInfo.Orientation with
    | R0 -> {X = float(sym.Pos.X) ; Y = float(sym.Pos.Y) ; H = float(sym.Compo.H) ; W = float(sym.Compo.W)}
    | R90 -> {X = float(sym.Pos.X) + float(fst offset); Y = float(sym.Pos.Y)  - float(snd offset) ; H = float(sym.Compo.W) ; W = float(sym.Compo.H)}
    | R180 -> {X = float(sym.Pos.X); Y = float(sym.Pos.Y); H = float(sym.Compo.H) ; W = float(sym.Compo.W)}
    | R270 -> {X = float(sym.Pos.X)  + float(fst offset); Y = float(sym.Pos.Y) - float(snd offset); H = float(sym.Compo.W) ; W = float(sym.Compo.H)}

let getBoundingBoxes (symModel: Model): Map<ComponentId, BoundingBox> =
    Map.map (fun sId (sym:Symbol) -> (getBoundingBoxofSymbol sym)) symModel.Symbols
    
let getOneBoundingBox (symModel: Model) (compid: ComponentId ): BoundingBox =
    let symb = Map.find compid symModel.Symbols
    getBoundingBoxofSymbol symb


//--------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS-------------------------------
// Helpers
let getSymbolPos (symbolModel: Model) compId =
    let symbol = Map.find compId symbolModel.Symbols
    symbol.Pos

/// This is quite slow, because it gets the whole maps.
/// It is used in getInputPortLocation for a single port!!
/// Bad
let getInputPortsPositionMap (model: Model) (symbols: Symbol list)  = 
    symbols
    |> List.collect (fun sym -> List.map (fun p -> sym,p) sym.Compo.InputPorts)
    |> List.map (fun (sym,port) -> (InputPortId port.Id, posAdd (getPortPosModel model port) (sym.Pos)))
    |> Map.ofList

/// This is quite slow, because it gets the whole maps.
/// It is used in getOutputPortLocation for a single port!!
/// Bad
let getOutputPortsPositionMap (model: Model) (symbols: Symbol list)  = //These function add the coordinates of the symbol too
    symbols
    |> List.collect (fun sym -> List.map (fun p -> sym,p) sym.Compo.OutputPorts)
    |> List.map (fun (sym,port) -> (OutputPortId port.Id , posAdd (getPortPosModel model port) (sym.Pos)))
    |> Map.ofList

///Returns the port object associated with a given portId
let getPort (symModel: Model) (portId: string) =
    symModel.Ports[portId]

///Returns all the port locations of the given components   
let getPortLocations (symbolModel: Model) (sIds: ComponentId list) = 
    let getSymbols = 
        symbolModel.Symbols 
        |> Map.filter (fun sId sym  -> List.contains sId sIds)
        |> Map.toList
        |> List.map snd
        
    let getInputPortMap = getInputPortsPositionMap symbolModel getSymbols
    let getOutputPortMap = getOutputPortsPositionMap symbolModel getSymbols
       
    getInputPortMap , getOutputPortMap

///Returns the location of an input portId  
let getInputPortLocation (model:Model) (portId: InputPortId)  = 
    let allSymbols =
        model.Symbols
        |> Map.toList
        |> List.map snd
        
    getInputPortsPositionMap model allSymbols 
    |> Map.find (portId)
    

//Returns the location of an output portId
let getOutputPortLocation (model:Model) (portId : OutputPortId) =
    let allSymbols =
        model.Symbols
        |> Map.toList
        |> List.map snd
        
    getOutputPortsPositionMap model allSymbols 
    |> Map.find (portId)     

///Returns the location of a given portId
let getOnePortLocation (symModel: Model) (portId : string) (pType: PortType)=
        match pType with
        | PortType.Input ->
            getInputPortLocation symModel (InputPortId portId)
        | PortType.Output ->
            getOutputPortLocation symModel (OutputPortId portId)   
            
/// Returns the location of a given portId, with better efficiency
/// This is still slow, the ports should be looked up from a map of ports
let getOnePortLocationNew (symModel: Model) (portId : string) (pType: PortType) : XYPos=
    symModel.Symbols
    |> Map.pick (fun key sym -> 
        let comp = sym.Compo
        if pType = PortType.Input then
            List.tryFind (fun (po:Port) -> po.Id = portId) comp.InputPorts
        else
            List.tryFind (fun (po:Port) -> po.Id = portId) comp.OutputPorts
        |> Option.map (fun port -> posAdd (getPortPosModel symModel port) (sym.Pos)))

/// Returns the side of a given portId
let getOnePortSide (symModel: Model) (portId : string) (pType: PortType) : Side=
    symModel.Symbols
    |> Map.pick (fun key sym -> 
        let comp = sym.Compo
        if pType = PortType.Input then
            List.tryFind (fun (po:Port) -> po.Id = portId) comp.InputPorts
        else
            List.tryFind (fun (po:Port) -> po.Id = portId) comp.OutputPorts
        |> Option.map (fun port -> getPortSideModel symModel port))

/// Returns the locations of a given input portId and output portId
let getTwoPortLocations (symModel: Model) (inPortId: InputPortId ) (outPortId: OutputPortId) =
    match inPortId, outPortId with
    | InputPortId inputId, OutputPortId outputId ->
        (getOnePortLocationNew symModel inputId PortType.Input, getOnePortLocationNew symModel outputId PortType.Output)

/// Returns the sides of a given input portId and output portId
let getTwoPortSides (symModel: Model) (inPortId: InputPortId ) (outPortId: OutputPortId) =
    match inPortId, outPortId with
    | InputPortId inputId, OutputPortId outputId ->
        (getOnePortSide symModel inputId PortType.Input, getOnePortSide symModel outputId PortType.Output)

/// Interface function to get componentIds of the copied symbols
let getCopiedSymbols (symModel: Model) : (ComponentId list) =
    symModel.CopiedSymbols
    |> Map.toList
    |> List.map fst

/// Function to filter out terminal non-letter characters.
/// Modified to capitalise labels
let filterString (string: string) = 
    string.ToUpper()
    |> Seq.rev
    |> Seq.skipWhile System.Char.IsDigit
    |> Seq.rev
    |> Seq.map System.Char.ToString
    |> String.concat ""
   
///Returns the number of the component label (i.e. the number 1 from IN1 or ADDER16.1)
let regex (str : string) = 
    let index = Regex.Match(str, @"\d+$")
    match index with
    | null -> 0
    | _ -> int index.Value

let getCompList compType listSymbols =
    match compType with 
       | Not | And | Or | Xor | Nand | Nor | Xnor -> 
            listSymbols
            |> List.filter (fun sym ->
                (sym.Compo.Type = Not || sym.Compo.Type = And 
                || sym.Compo.Type = Or || sym.Compo.Type = Xor
                || sym.Compo.Type = Nand || sym.Compo.Type = Nor
                || sym.Compo.Type = Xnor)
                )
       | DFF | DFFE -> 
            listSymbols
            |> List.filter (fun sym ->
                (sym.Compo.Type = DFF || sym.Compo.Type = DFFE))
       //The following components require this pattern matching in order to correctly identify all of the components in the circuit of that type
       //Normally this is because they are defined by a width as well as a type
       | Register _ | RegisterE _ ->
            listSymbols
            |> List.filter (fun sym ->
                match sym.Compo.Type with 
                | Register _ | RegisterE _ -> true
                | _ -> false)
       | Constant1 _ ->
            listSymbols
            |> List.filter (fun sym ->
                match sym.Compo.Type with 
                | Constant1 _ -> true
                | _ -> false)
       | Input _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Input _ -> true
               | _ -> false)
       | Output _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Output _ -> true
               | _ -> false)
       | Viewer _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Viewer _ -> true
               | _ -> false)
       | BusSelection _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | BusSelection _ -> true
               | _ -> false)
       | BusCompare _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | BusCompare _ -> true
               | _ -> false)
       | NbitsAdder _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | NbitsAdder _ -> true
               | _ -> false)
       | NbitsXor _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | NbitsXor _ -> true
               | _ -> false)
       | AsyncROM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | AsyncROM1 _ -> true
               | _ -> false)
       | ROM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | ROM1 _ -> true
               | _ -> false)
       | RAM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | RAM1 _ -> true
               | _ -> false)
       | AsyncRAM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | AsyncRAM1 _ -> true
               | _ -> false)

       | _ ->
            listSymbols
            |> List.filter (fun sym -> sym.Compo.Type = compType)

let getIndex listSymbols compType =
    let symbolList = 
        getCompList compType listSymbols

    match compType with
    | MergeWires | SplitWire _ -> ""
    | _ ->
        if List.isEmpty symbolList then 1 
        else symbolList
            |> List.map (fun sym -> regex sym.Compo.Label)
            |> List.max
            |> (+) 1
        |> string

///Generates the number to be put in the title of symbols  
let labelGenNumber (model: Model) (compType: ComponentType) (label : string) = 
    let listSymbols = List.map snd (Map.toList model.Symbols) 
    match compType with
    | IOLabel -> label
    | _ -> filterString label + (getIndex listSymbols compType)

///Generates the label for a component type
let generateLabel (model: Model) (compType: ComponentType) : string =
    labelGenNumber model compType (prefix compType)

/// Interface function to paste symbols. Is a function instead of a message because we want an output
/// Currently drag-and-drop
let pasteSymbols (symModel: Model) (mPos: XYPos) : (Model * ComponentId list) =
    let createNewSymbol (basePos: XYPos) ((currSymbolModel, pastedIdsList) : Model * ComponentId List) (oldSymbol: Symbol): Model * ComponentId List =
        let newId = JSHelpers.uuid()
        let posDiff = posDiff oldSymbol.Pos basePos
        let newPos = posAdd posDiff mPos
        
        let pastedSymbol =
            { oldSymbol with
                Id = ComponentId newId
                Compo = makeComp newPos oldSymbol.Compo.Type newId (labelGenNumber { symModel with Symbols = currSymbolModel.Symbols } oldSymbol.Compo.Type oldSymbol.Compo.Label) oldSymbol.Compo.SymbolInfo.Orientation oldSymbol.Compo.SymbolInfo.Flipped// TODO: Change label later
                Pos = newPos
                ShowInputPorts = false
                ShowOutputPorts = false }
             
        let newSymbolMap = currSymbolModel.Symbols.Add ((ComponentId newId), pastedSymbol) // List needs to be in this order
        let newPorts = addToPortModel currSymbolModel pastedSymbol.Compo
        { currSymbolModel with Symbols = newSymbolMap; Ports = newPorts }, pastedIdsList @ [ pastedSymbol.Id ]
        
    let oldSymbolsList =
        symModel.CopiedSymbols
        |> Map.toList
        |> List.map snd

    match List.sortBy (fun sym -> sym.Pos.X) oldSymbolsList with
    | baseSymbol :: _ ->
        let basePos = posAdd baseSymbol.Pos { X = (float baseSymbol.Compo.W) / 2.0; Y = (float baseSymbol.Compo.H) / 2.0 }
        
        ((symModel, []), oldSymbolsList) ||> List.fold (createNewSymbol basePos)
    | [] -> symModel, []

    
/// Given two componentId list of same length and input / output ports that are in list 1, return the equivalent ports in list 2.
/// ComponentIds at same index in both list 1 and list 2 need to be of the same ComponentType
/// CompIds1 need to be in model.CopiedSymbols
let getEquivalentCopiedPorts (model: Model) (copiedIds) (pastedIds) (InputPortId copiedInputPort, OutputPortId copiedOutputPort) =
    let findEquivalentPorts compId1 compId2 =
        let copiedComponent = model.CopiedSymbols[compId1].Compo
        let pastedComponent = model.Symbols[compId2].Compo // TODO: These can be different for an output gate for some reason.
        
        let tryFindEquivalentPort (copiedPorts: Port list) (pastedPorts: Port list) targetPort =
            if copiedPorts.Length = 0 || pastedPorts.Length = 0
            then None
            else
                match List.tryFindIndex ( fun (port: Port) -> port.Id = targetPort ) copiedPorts with
                | Some portIndex -> 

                    Some pastedPorts[portIndex].Id // Get the equivalent port in pastedPorts. Assumes ports at the same index are the same (should be the case unless copy pasting went wrong).
                | _ -> None
        
        let pastedInputPortId = tryFindEquivalentPort copiedComponent.InputPorts pastedComponent.InputPorts copiedInputPort
        let pastedOutputPortId = tryFindEquivalentPort copiedComponent.OutputPorts pastedComponent.OutputPorts copiedOutputPort
    
        pastedInputPortId, pastedOutputPortId
        
    let foundPastedPorts =
        List.zip copiedIds pastedIds
        |> List.map (fun (compId1, compId2) -> findEquivalentPorts compId1 compId2)
    
    let foundPastedInputPort = List.collect (function | Some a, _ -> [a] | _ -> []) foundPastedPorts
    let foundPastedOutputPort = List.collect (function | _, Some b -> [b] | _ -> []) foundPastedPorts
    
    match foundPastedInputPort, foundPastedOutputPort with 
    | [pastedInputPort], [pastedOutputPort] -> Some (pastedInputPort, pastedOutputPort) 
    | _ -> None // If either of source or target component of the wire was not copied then we discard the wire
  
 
/// Given a model return a model with a new Symbol and also the component id
let addSymbol (model: Model) pos compType lbl =
    let newSym = createNewSymbol pos compType lbl
    let newPorts = addToPortModel model newSym.Compo
    let newSymModel = Map.add newSym.Id newSym model.Symbols
    { model with Symbols = newSymModel; Ports = newPorts }, newSym.Id

// Helper function to change the number of bits expected in a port of each component type
let changeNumberOfBitsf (symModel:Model) (compId:ComponentId) (newBits : int) =
    
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
        | Input _ -> Input newBits
        | Output _ -> Output newBits
        | Viewer _ -> Viewer newBits
        | NbitsAdder _ -> NbitsAdder newBits
        | NbitsXor _ -> NbitsXor newBits
        | Register _ -> Register newBits
        | RegisterE _ -> RegisterE newBits
        | SplitWire _ -> SplitWire newBits
        | BusSelection (_,b) -> BusSelection (newBits,b)
        | BusCompare (_,b) -> BusCompare (newBits,b)
        | Constant1 (_,b,txt) -> Constant1 (newBits,b,txt)
        | c -> c

    let newcompo = {symbol.Compo with Type = newcompotype}
    {symbol with Compo = newcompo}

// Helper function to change the number of bits expected in the LSB port of BusSelection and BusCompare
let changeLsbf (symModel:Model) (compId:ComponentId) (newLsb:int64) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(newLsb))
        | BusCompare (w, _) -> BusCompare (w, uint32(newLsb)) 
        | Constant1(w, _,txt) -> Constant1 (w, newLsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Compo with Type = newcompotype}
    {symbol with Compo = newcompo}

let changeConstantf (symModel:Model) (compId:ComponentId) (constantVal:int64) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Compo with Type = newcompotype}
    printfn "Changing symbol to: %A" newcompotype
    {symbol with Compo = newcompo}
    
/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | DeleteSymbols compList ->
        let newSymbols = List.fold (fun prevModel sId -> Map.remove sId prevModel) model.Symbols compList
        { model with Symbols = newSymbols }, Cmd.none //filters out symbol with a specified id

    | AddSymbol (pos,compType, lbl) ->
        let (newModel, _) = addSymbol model pos compType lbl
        newModel, Cmd.none

    | CopySymbols compIds ->
        let copiedSymbols = Map.filter (fun compId _ -> List.contains compId compIds) model.Symbols
        { model with CopiedSymbols = copiedSymbols }, Cmd.none

    | RotateSymbols compIds ->   // processing of rotation message
        let getOrientation (rot: Rotation) : Rotation =
            match rot with 
            | R0 -> R90
            | R90 -> R180
            | R180 -> R270
            | R270 -> R0

        let newSymbols = 
            model.Symbols |> Map.map (fun compId symbol ->
                match List.contains compId compIds with
                | false -> symbol
                | true -> {symbol with Compo={symbol.Compo with SymbolInfo={symbol.Compo.SymbolInfo with Orientation = (getOrientation symbol.Compo.SymbolInfo.Orientation)}}})              
        
        {model with Symbols = newSymbols }, Cmd.none

    | FlipSymbols compIds -> //// processing of flipping message
        let newSymbols = 
            model.Symbols |> Map.map (fun compId symbol ->
                match List.contains compId compIds with
                | false -> symbol
                | true -> {symbol with Compo={symbol.Compo with SymbolInfo = {symbol.Compo.SymbolInfo with Flipped = not symbol.Compo.SymbolInfo.Flipped}}})                    

        { model with Symbols = newSymbols }, Cmd.none

    | ChangeSelectedPort compIds->
        let newSymbols = 
            model.Symbols |> Map.map (fun compId symbol ->
                match List.contains compId compIds with
                | false -> symbol
                | true -> {symbol with SelectedPort = changePortSelection symbol})                    

        { model with Symbols = newSymbols }, Cmd.none

    | MoveSelectedPort compIds->
        let newSymbols = 
            model.Symbols |> Map.map (fun compId symbol ->
                match List.contains compId compIds with
                | false -> symbol
                | true ->
                    match symbol.SelectedPort with
                    | Some name -> {symbol with TIPortMap = movePort symbol name}
                    | None -> symbol)              

        { model with Symbols = newSymbols }, Cmd.none

    | ShowAllInputPorts ->
        { model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = true; ShowOutputPorts = false}) model.Symbols },
        Cmd.none

    | ShowAllOutputPorts ->
        {model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = true}) model.Symbols },
        Cmd.none

    | DeleteAllPorts ->
        { model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = false}) model.Symbols },
        Cmd.none //demo

    | ShowPorts compList -> //show ports of one component (shown in demo for a random component, sheet gives list in group phace)  find showPorts in other interfaces (above)
        let resetSymbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = false}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with ShowInputPorts = true; ShowOutputPorts = true} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | MoveSymbols (compList, move) -> 
        let resetSymbols = Map.map (fun _ sym -> { sym with Moving = false}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId ->
                let (newCompo: Component) = {model.Symbols[sId].Compo with X = int (model.Symbols[sId].Pos.X + move.X);Y = int (model.Symbols[sId].Pos.Y + move.Y )}
                Map.add sId {model.Symbols[sId] with Moving = true; Pos = {X = (model.Symbols[sId].Pos.X + move.X);Y = (model.Symbols[sId].Pos.Y + move.Y)} ; Compo = newCompo} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | SymbolsHaveError compList ->
        let resetSymbols = Map.map (fun _ sym -> {sym with Colour = "Lightgray"}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | SelectSymbols compList -> //select a symbol (shown in demo for a random component, sheet gives list in group phase)
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0; SelectedPort = None}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "lightgreen"; SelectedPort = portSelectInitial resetSymbols[sId].Compo} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none  

    | ErrorSymbols (errorCompList,selectCompList,isDragAndDrop) -> 
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let selectSymbols =
            List.fold (fun prevSymbols sId -> 
                            if not isDragAndDrop then 
                                Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols
                            else 
                                Map.add sId { resetSymbols[sId] with Opacity = 0.2 } prevSymbols
                        ) resetSymbols selectCompList
        let newSymbols = 
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols) selectSymbols errorCompList)
        { model with Symbols = newSymbols }, Cmd.none 
        
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags

    | ChangeLabel (sId, newLabel) ->
        let tempsym = Map.find sId model.Symbols
        let newcompo = {tempsym.Compo with Label = newLabel}
        let addsym = {tempsym with Compo = newcompo}
        { model with Symbols = Map.add sId addsym model.Symbols }, Cmd.none

    | PasteSymbols compList ->
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId { model.Symbols[sId] with Opacity = 0.4 } prevSymbols) model.Symbols compList)
        { model with Symbols = newSymbols }, Cmd.none  
    
    | ColorSymbols (compList, colour) -> 
        let newSymbols = 
            Map.map (fun sId sym -> if List.contains sId compList then {sym with Colour = string colour} else sym) model.Symbols
        { model with Symbols = newSymbols }, Cmd.none 
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newsymbol = changeNumberOfBitsf model compId newBits
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ChangeLsb (compId, newLsb) -> 
        let newsymbol = changeLsbf model compId newLsb
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none

    | ChangeConstant (compId, newVal, newText) -> 
        let newsymbol = changeConstantf model compId newVal newText
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ResetModel -> { model with Symbols = Map.empty; Ports = Map.empty }, Cmd.none
    
    | LoadComponents comps ->
        let compIdsWithSymbols =
            comps
            |> List.map ( fun comp -> (
                                        let xyPos = {X = float comp.X; Y = float comp.Y}
                                        let (h,w) =
                                            if comp.H = -1 && comp.W = -1 then
                                                let comp' = makeComp xyPos comp.Type comp.Id comp.Label comp.SymbolInfo.Orientation comp.SymbolInfo.Flipped
                                                comp'.H,comp'.W
                                            else
                                                comp.H, comp.W
                                        ComponentId comp.Id,
                                        { Pos = xyPos
                                          ShowInputPorts = false //do not show input ports initially
                                          ShowOutputPorts = false //do not show output ports initially
                                          Colour = "lightgrey"     // initial color 
                                          Id = ComponentId comp.Id
                                          Compo = {comp with H=h ; W = w}
                                          Opacity = 1.0
                                          Moving = false
                                          InWidth0 = None
                                          InWidth1 = None
                                          TIPortMap = initialisePortMap comp.Type
                                          SelectedPort = None
                                        }
                                        ))
        let symbolList =
            compIdsWithSymbols
            |> List.map snd

        let symbolMap =
            compIdsWithSymbols   
            |> Map.ofList
        
        let folder currModel sym =
            { currModel with Ports = addToPortModel currModel sym.Compo }
            
        let newModel = ( model, symbolList ) ||> List.fold folder
        { newModel with Symbols = symbolMap }, Cmd.none
 
    | WriteMemoryLine (compId, addr, value) ->
        let symbol = model.Symbols[compId]
        let comp = symbol.Compo
        
        let newCompType =
            match comp.Type with
            | RAM1 mem -> RAM1 { mem with Data = Map.add addr value mem.Data }
            | AsyncRAM1 mem -> AsyncRAM1 { mem with Data = Map.add addr value mem.Data }
            | ROM1 mem -> ROM1 { mem with Data = Map.add addr value mem.Data }
            | AsyncROM1 mem -> AsyncROM1 { mem with Data = Map.add addr value mem.Data }
            | _ -> comp.Type
        
        let newComp = { comp with Type = newCompType }
        
        let newSymbols = Map.add compId { symbol with Compo = newComp } model.Symbols
        
        { model with Symbols = newSymbols }, Cmd.none
    | WriteMemoryType (compId, memory) ->
        let symbol = model.Symbols[compId]
        let comp = symbol.Compo       
        let newCompType =
            match comp.Type with
            | RAM1 mem | AsyncRAM1 mem -> memory
            | ROM1 mem -> memory
            | AsyncROM1 mem -> memory
            | _ -> 
                printfn $"Warning: improper use of WriteMemoryType on {comp} ignored"
                comp.Type
        
        let newComp = { comp with Type = newCompType }
        
        let newSymbols = Map.add compId { symbol with Compo = newComp } model.Symbols
        
        { model with Symbols = newSymbols }, Cmd.none
        
// ----------------------interface to Issie----------------------------- //
let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    symModel.Symbols[sId].Compo

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)
