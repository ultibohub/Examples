program ultiboappfonts;

{
 This example demonstrates how to use the VGShapesLoadAppFont() function to load
 application fonts into VGShapes, and how to access them using multiple layers.
}

{$mode objfpc}{$H+}

uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  VGShapes,
  OpenVG,
  dispmanx,
  Shell,
  ShellUpdate,
  RemoteShell,
  ShellFilesystem,
  console
  ;

{This file contains the actual font data. To generate this data, use the ultibo
 edition of the font2openvg at https://github.com/ric355/font2openvg
 You do not need to use the unit file that is generated, you only need to
 $include the .inc file}

{$include lcdphone.inc}
{$include roughbrush.inc}
{$include blazed.inc}

procedure initgraphics(var Width, Height : integer);
var
 layer1font : PVGShapesFontInfo;
begin
 {Initialize OpenVG and load an application font}

 {initialize layer 0}
 VGShapesSetLayer(0);
 VGShapesInit(Width,Height);

 {initialize layer  1, with transparency enabled so the layer below shows through}
 VGShapesSetLayer(1);
 VGShapesInit(Width,Height, DISPMANX_FLAGS_ALPHA_FROM_SOURCE);

 {load an application font and name it lcd}
 {we want this font to be displayable on more than one layer, so we must load
 it for each layer. Note that the names do not have to be the same on the two
 layers for the same font, it's just more logical to do that for this demo.}

 {layer 0}
 VGShapesSetLayer(0);
 VGShapesLoadAppFont('lcd',
         @lcdphone_glyphPoints,
         @lcdphone_glyphPointIndices,
         @lcdphone_glyphInstructions,
         @lcdphone_glyphInstructionIndices,
         @lcdphone_glyphInstructionCounts,
         @lcdphone_glyphAdvances,
         @lcdphone_characterMap,
         lcdphone_glyphCount,
         lcdphone_descender_height,
         lcdphone_font_height);

 VGShapesLoadAppFont('roughbrush',
         @roughbrush_glyphPoints,
         @roughbrush_glyphPointIndices,
         @roughbrush_glyphInstructions,
         @roughbrush_glyphInstructionIndices,
         @roughbrush_glyphInstructionCounts,
         @roughbrush_glyphAdvances,
         @roughbrush_characterMap,
         roughbrush_glyphCount,
         roughbrush_descender_height,
         roughbrush_font_height);


 {layer 1}
 VGShapesSetLayer(1);

 {the add function returns the font - you can use that return value in other calls
 instead of calling GetAppFontByName if you prefer, but the byname function enables
 lookups from other data such as configuration files or whatever}
 layer1font := VGShapesLoadAppFont('lcd',
         @lcdphone_glyphPoints,
         @lcdphone_glyphPointIndices,
         @lcdphone_glyphInstructions,
         @lcdphone_glyphInstructionIndices,
         @lcdphone_glyphInstructionCounts,
         @lcdphone_glyphAdvances,
         @lcdphone_characterMap,
         lcdphone_glyphCount,
         lcdphone_descender_height,
         lcdphone_font_height);

 {load a second font, different to the second font loaded on the first layer}
 VGShapesLoadAppFont('blazed',
         @blazed_glyphPoints,
         @blazed_glyphPointIndices,
         @blazed_glyphInstructions,
         @blazed_glyphInstructionIndices,
         @blazed_glyphInstructionCounts,
         @blazed_glyphAdvances,
         @blazed_characterMap,
         blazed_glyphCount,
         blazed_descender_height,
         blazed_font_height);

end;


var
 Width, Height : integer;
 WindowHandle : TWindowHandle;
 i : VGFloat;
 starty, endy : VGFloat;
 endx : VGFloat;
 lcdfont : PVGShapesFontInfo;

begin
 SHELL_UPDATE_LOCAL_PATH:='c:\clusterkernel\';

 {we're going to put a console behind the openvg layers; you'll see it when openvg is closed
  near the end of the execution.}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
 ConsoleWindowWriteln(WindowHandle, 'Ultibo VGLayers Demo application');

 {start openvg}
 InitGraphics(Width, Height);

 {start frame, layer 0}
 VGShapesSetLayer(0);
 VGShapesStart(Width, Height);

 {This write to the console won't actually be visible until after OpenVG is closed near the end of the
 application's execution}
 ConsoleWindowWriteln(WindowHandle, 'OpenVG Width and Height are ('+inttostr(Width) +',' + inttostr(Height) + ')');

 {middle of the window}
 VGShapesTextMid(Width div 2, Height div 2, 'LAYER 0 SANS SERIF', VGShapesSansTypeface, 70);

 {Note that here we use 'getappfontbyname' directly instead of storing the font as we do later. It is slightly
  less efficient due to having to search for the matching font. Note also that the pointer to the font structure
  returned is not the same between layers 1 and 2, since the fonts are loaded into the GPU multiple times (on demand)
  on a per layer basis. Thus if storing font pointers you must take care to use the pointer associated with the
  layer you got it from when drawing text, otherwise the text just won't appear}

 VGShapesTextMid(Width div 2, Height div 2 - 100, 'LAYER 0 LCD PHONE', VGShapesGetAppFontByName('lcd'), 70);

 {the internally loaded fonts are also available via VGShapesGetAppFontByName
  The VGShapes unit has three constants to represent them
  You can still call the functions to get them if you prefer as per the layer 0
  sans serif text above}
 VGShapesTextMid(Width div 2, Height div 2 - 200, 'LAYER 0 SERIF', VGShapesGetAppFontByName(VGSHAPES_FONTNAME_SERIF), 70);

 VGShapesTextMid(Width div 2, Height div 2 - 300, 'LAYER 0 Rough Brush', VGShapesGetAppFontByName('roughbrush'), 70);

 VGShapesEnd;

 {start frame, layer 1, with transparency}
 VGShapesSetLayer(1);

 {get the loaded font using its name, default to current layer by not specifying a layer id}
 {note we can only use this font pointer on this current layer (1)}
 lcdfont := VGShapesGetAppFontByName('lcd');
 starty := VGShapesTextHeight(lcdfont, 70);
 endy := Height - starty;

 {draw some text and scroll it upwards, demonstrating that it doesn't overwrite the other layer}
 i := starty;
 while i < endy do
 begin
   VGShapesStart(Width, Height, True);

   VGShapesTextMid(Width div 2, i, 'LAYER 1 LCD PHONE', lcdfont, 70);

   VGShapesTextMid(Width div 2, Height div 2 - 300, 'LAYER 1 Blazed Font', VGShapesGetAppFontByName('blazed'), 70);

   VGShapesEnd;

   i := i + 1;
 end;

 {now move back to layer 0 and scroll some text sideways}
 VGShapesSetLayer(0);

 {note that when we do this, the text previously on layer 0 will disappear.
  This is because in OpenVG, calling VGShapesStart clears the display ready for a new frame}
 endx := Width - VGShapesTextWidth('LAYER 0 LCD PHONE', VGShapesGetAppFontByName('lcd'), 70);
 i := 0;

 while i < endx do
 begin
   VGShapesStart(Width, Height, False);

   {red, solid fill}
   VGShapesFill(255, 0, 0, 255);

   {note that this text will appear behind the black text drawn above, because it is
   on layer zero. This is more obvious when the two colours are different}
   VGShapesText(i, endy, 'LAYER 0 LCD PHONE', VGShapesGetAppFontByName('lcd'), 70);

   VGShapesEnd;

   i := i + 1;
 end;

 {close everything down}
 VGShapesSetLayer(0);
 VGShapesFinish;

 {we must finish the other layer as well}
 VGShapesSetLayer(1);
 VGShapesFinish;

 ConsoleWindowWriteln(WindowHandle, 'Demo finished.');

end.

