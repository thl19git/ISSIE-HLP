# Team 1 Analysis Report

## BusWire
- `makeInitialWireVerticesList` creates arrays for x and y coordinates of for all 7 segments of any possible wire configuration between any possible pair of input-output port orientations (left, right, top and bottom with left, right, top and bottom)
- `makeInitialWireVerticesList` uses `portSides` parameter to allow the wires to route to arbitrary Symbol port orientations through the method explained above
- Added `getPortSideCase` as a helper function to `makeInitialWireVerticesList` to assign a number to each of the possible port orientations. `portSides` assigns a case number for any possible configuration (28 cases in total: 0 -> 27)
- `xyVerticesToSegment` takes all the cases from `makeInitialWireVerticesList` and provides a wire configuration for every case; the order of Horizontal and Vertical is strategically chosen such that it does not create issues in `moveSegment` when moving a segment
- `moveSegment` now takes into account all possible port orientations by using `isSafeMove` which does not allow a horizontal or vertical wire to cross a symbol with vertical orientation or horizontal orientation, respectively.
- `moveSegment` includes stickiness for very short segments
- `moveSegment` is adjusting the neighbour segments of the segment which is in the process of manual routing.
- Added `Style` property to `Model` to store the desired wire rendering style
- `Msg` now contains `UpdateStyle`, which is used for changing the wire rendering style from the top menu
- Removed many redundant functions in Part 2 

## Symbol
- Updated `Symbol` type with `SelectedPort` field to reflect the currently selected port
- Updated `Symbol` type with `TIPortMap` field, to store information about the display side and order for each port in the component
- Updated `Component` type with `SymbolInfo` field, which records orientation and flippedness. In `Component` rather than `Symbol` so it is saved with the sheet
- Updated `Msg` type with the following messages:
    - `RotateSymbols` which changes the `Compo.SymbolInfo.Orientation` field for selected symbols
    - `FlipSymbols` which changes the `Compo.SymbolInfo.Flipped` field for selected symbols
    - `ChangeSelectedPort` which changes the `SelectedPort` field for selected symbol, does not work with multiple selected symbols to avoid confusion
    - `MoveSelectedPort` which changes the `TIPortMap` field of the symbol
- Added cases into `update` function to perform the aformentioned operations with use of helper functions
- Explanations of algorithms / trickier functions:
    - Symbol rotation and flipping makes extensive use of the helper function `flipAndRotatePoint`. This is implemented using matrix transformations. The midpoint of a symbol is defined as (w/2, h/2). Rotation is about that midpoint, whilst flipping is a reflection in the line y = h/2. To apply these transformations, the point is first transformed such that the midpoint becomes the origin. A flip then becomes a reflection in the x-axis, whilst rotation is about the origin. The transformation is finished by translating the midpoint from the origin to its initial position.
    - Ports are selected by using the shortcut Ctrl+P. Pressing this changes the selected port by highlighting the next port (this is the next port going clockwise if the symbol hasn’t been flipped, or anticlockwise if the symbol has been flipped). The function responsible for handling this is `changePortSelection`. This is implemented by first checking to see if the currently selected port is the last port on that side (highest index). If it isn’t the next port to be selected is the port at index + 1 on the same side. If the selected port is the last port, then the next side with ports on it is identified, and the next selected port is the port at index 0 on that side. Care has been taken to ensure that this also covers the scenario where all ports of a symbol are on the same side.
    - The coordinates of a port are found by the `getPortPosCoords` function, which evenly spreads ports on a side of a symbol according to the port index and the number of ports on that side. Care has been taken to ensure this still applies for the sloped edges of multiplexers and demultiplexers, by introducing an offset that is 0 for all other component types.
    - The label of a symbol is rendered by the `addTitle` function. This first checks to see if there are any sides without ports. If there are no sides without ports, then the label is placed above the top-left corner of the symbol. If the top side is free of ports, then the label will be placed in the centre above the symbol. Else, the label will be placed next to another free side.
    - Ports are moved using the `movePort` function. If the selected port is not at index 0 on that side, it switches position with the adjacent port at a lower index. When it is at index 0, the port's side rotates, its index becomes one higher than the highest index on the new side, and all the port indices on the old side are decremented to centre the ports.


## Sheet
- Updated `KeyboardMsg` type with new hotkeys: 
    <ul>
        <li><b>Ctrl+R:</b> for symbol rotation</li>
        <li><b>Ctrl+F:</b> for symbol flipping</li>
        <li><b>Ctrl+P:</b> for port selection</li>
        <li><b>Ctrl+M:</b> for port movement</li>
    </ul>

- In `update` function, updated `ManualKeyDown` message processing to recognise new hotkeys

- In `update` function, created cases for new hotkeys, which will send the respective messages to `Symbol.fs`, and wire updating messages to `BusWire.fs`

## Renderer
- Added `styleMenu` which sends `UpdateStyle` messages to `BusWire`
