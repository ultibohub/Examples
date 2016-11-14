program BouncingBoxes;

{$mode objfpc}{$H+}{$inline on}

{ Example 16 Bouncing Boxes                                                    }
{                                                                              }
{ A simple demonstration of animation using the framebuffer device in Ultibo   }
{ with a virtual width and height that allows fast swapping between pages.     }
{                                                                              }
{ The example creates 200 colored boxes and moves them around the screen so    }
{ that they appear to bounce off the sides.                                    }
{                                                                              }
{ Even though the example is doing all of the drawing using nothing but the    }
{ CPU it is able to maintain 60 frames per second on any size screen due to    }
{ some simple optimizations.                                                   }
{                                                                              }
{ There are lots of variables that can be changed within the code to make      }
{ different effects like the speed and number of boxes, colors, sizes and more.}
{ Experiment on your own to see what you can create.                           }
{                                                                              }
{ Warning, the animation is very hypnotic. If you're not careful you can find  }
{ yourself watching it for hours!                                              }
{                                                                              }
{                                                                              }
{ This example was originally created by Raspberry Compote for an excellent    }
{ series entitled 'Low-level Graphics on Raspberry Pi'.                        }
{                                                                              }
{  You can find the original series of articles here:                          }
{   http://raspberrycompote.blogspot.com.au/2012/12/low-level-graphics-on-raspberry-pi-part_9509.html }
{                                                                              }
{  And the original C source code here:                                        }
{   https://github.com/rst-/raspberry-compote/blob/master/fb/fbtestXIII.c      }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel.img file to an SD card along with the         }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi A/B/A+/B+/Zero version                                         }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
 RaspberryPi,  {The RaspberryPi unit gives us all the relevant drivers}
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 Framebuffer,  {Most of the functions we need come from the Framebuffer unit}
 Console,
 SysUtils;
 

{Some variables used by the animation} 
var
 PageSize:Integer;
 CurrentPage:Integer;
 BufferStart:Pointer;
 FramebufferDevice:PFramebufferDevice;
 FramebufferProperties:TFramebufferProperties;
 

{A couple of constants, what happens if you change NUM_ELEMENTS?}
const
 NUM_ELEMENTS = 200;
 RANDOM_MAX:LongInt = 2147483647;
 
 
{Some array variables to track the position of each box} 
var 
 ElementX:array[0..NUM_ELEMENTS - 1] of Integer;
 ElementY:array[0..NUM_ELEMENTS - 1] of Integer;
 ElementDirX:array[0..NUM_ELEMENTS - 1] of Integer;
 ElementDirY:array[0..NUM_ELEMENTS - 1] of Integer;
 
 
{The "standard" VGA Mode 13h palette for 8 bit colors (See: https://en.wikipedia.org/wiki/Mode_13h)} 
const
 VGAPalette:TFramebufferPalette = (
  Start:0;
  Count:256;
  Entries:
  ($FF000000,$FF0000AA,$FF00AA00,$FF00AAAA,$FFAA0000,$FFAA00AA,$FFAA5500,$FFAAAAAA,$FF555555,$FF5555FF,$FF55FF55,$FF55FFFF,$FFFF5555,$FFFF55FF,$FFFFFF55,$FFFFFFFF,
   $FF000000,$FF141414,$FF202020,$FF2C2C2C,$FF383838,$FF444444,$FF505050,$FF606060,$FF707070,$FF808080,$FF909090,$FFA0A0A0,$FFB4B4B4,$FFC8C8C8,$FFE0E0E0,$FFFCFCFC,
   $FF0000FC,$FF4000FC,$FF7C00FC,$FFBC00FC,$FFFC00FC,$FFFC00BC,$FFFC007C,$FFFC0040,$FFFC0000,$FFFC4000,$FFFC7C00,$FFFCBC00,$FFFCFC00,$FFBCFC00,$FF7CFC00,$FF40FC00,
   $FF00FC00,$FF00FC40,$FF00FC7C,$FF00FCBC,$FF00FCFC,$FF00BCFC,$FF007CFC,$FF0040FC,$FF7C7CFC,$FF9C7CFC,$FFBC7CFC,$FFDC7CFC,$FFFC7CFC,$FFFC7CDC,$FFFC7CBC,$FFFC7C9C,
   $FFFC7C7C,$FFFC9C7C,$FFFCBC7C,$FFFCDC7C,$FFFCFC7C,$FFDCFC7C,$FFBCFC7C,$FF9CFC7C,$FF7CFC7C,$FF7CFC9C,$FF7CFCBC,$FF7CFCDC,$FF7CFCFC,$FF7CDCFC,$FF7CBCFC,$FF7C9CFC,
   $FFB4B4FC,$FFC4B4FC,$FFD8B4FC,$FFE8B4FC,$FFFCB4FC,$FFFCB4E8,$FFFCB4D8,$FFFCB4C4,$FFFCB4B4,$FFFCC4B4,$FFFCD8B4,$FFFCE8B4,$FFFCFCB4,$FFE8FCB4,$FFD8FCB4,$FFC4FCB4,
   $FFB4FCB4,$FFB4FCC4,$FFB4FCD8,$FFB4FCE8,$FFB4FCFC,$FFB4E8FC,$FFB4D8FC,$FFB4C4FC,$FF000070,$FF1C0070,$FF380070,$FF540070,$FF700070,$FF700054,$FF700038,$FF70001C,
   $FF700000,$FF701C00,$FF703800,$FF705400,$FF707000,$FF547000,$FF387000,$FF1C7000,$FF007000,$FF00701C,$FF007038,$FF007054,$FF007070,$FF005470,$FF003870,$FF001C70,
   $FF383870,$FF443870,$FF543870,$FF603870,$FF703870,$FF703860,$FF703854,$FF703844,$FF703838,$FF704438,$FF705438,$FF706038,$FF707038,$FF607038,$FF547038,$FF447038,
   $FF387038,$FF387044,$FF387054,$FF387060,$FF387070,$FF386070,$FF385470,$FF384470,$FF505070,$FF585070,$FF605070,$FF685070,$FF705070,$FF705068,$FF705060,$FF705058,
   $FF705050,$FF705850,$FF706050,$FF706850,$FF707050,$FF687050,$FF607050,$FF587050,$FF507050,$FF507058,$FF507060,$FF507068,$FF507070,$FF506870,$FF506070,$FF505870,
   $FF000040,$FF100040,$FF200040,$FF300040,$FF400040,$FF400030,$FF400020,$FF400010,$FF400000,$FF401000,$FF402000,$FF403000,$FF404000,$FF304000,$FF204000,$FF104000,
   $FF004000,$FF004010,$FF004020,$FF004030,$FF004040,$FF003040,$FF002040,$FF001040,$FF202040,$FF282040,$FF302040,$FF382040,$FF402040,$FF402038,$FF402030,$FF402028,
   $FF402020,$FF402820,$FF403020,$FF403820,$FF404020,$FF384020,$FF304020,$FF284020,$FF204020,$FF204028,$FF204030,$FF204038,$FF204040,$FF203840,$FF203040,$FF202840,
   $FF2C2C40,$FF302C40,$FF342C40,$FF3C2C40,$FF402C40,$FF402C3C,$FF402C34,$FF402C30,$FF402C2C,$FF40302C,$FF40342C,$FF403C2C,$FF40402C,$FF3C402C,$FF34402C,$FF30402C,
   $FF2C402C,$FF2C4030,$FF2C4034,$FF2C403C,$FF2C4040,$FF2C3C40,$FF2C3440,$FF2C3040,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000)
  );

 
{Since this example was originally ported from C source code a couple of optimizations
 were needed in order to get the frame rate high enough for the animation to run smoothly.
 
 Out of interest the 'orignal' functions plus another variation have been left here to
 show the optimization process.
 
 The functions PutPixel() and FillRect1() were ported directly from the C source but under
 Free Pascal the calculation of the PixelOffset value for each pixel in the frame is far
 too slow (it's possible that the C compiler did extra optimization on the original).
 
 The FillRect2() function is an intermediate step which improved the rate dramatically but
 still needed a little more so it was replaced by the final FillRect() which delivers the
 full 60 frames per second at any screen size} 
 
 
{Helper function to 'plot' a pixel in given color} 
(*procedure PutPixel(X,Y,Color:Integer); inline;
var
 PixelOffset:Cardinal;
begin
 {Calculate the pixel's byte offset inside the buffer}
 PixelOffset:=X + (Y * FramebufferProperties.Pitch);
 
 {Offset by the current buffer start}
 PixelOffset:=PixelOffset + (CurrentPage * PageSize);
 
 {This is about the same as 'BufferStart[PixelOffset]:=Color'}
 PByte(BufferStart + PixelOffset)^:=Color;
end;*)


{Helper function to draw a rectangle in given color}
(*procedure FillRect1(X,Y,Width,Height,Color:Integer); inline;
var
 CurrentX:Integer;
 CurrentY:Integer;
begin
 {}
 {Draw each row of pixels}
 for CurrentY:=0 to Height - 1 do
  begin
   {Draw the pixel for each column}
   for CurrentX:=0 to Width - 1 do
    begin
     {Draw the pixel}
     PutPixel(X + CurrentX,Y + CurrentY,Color);
    end;
  end;
end;*)


{Helper function to draw a rectangle in given color}
(*procedure FillRect2(X,Y,Width,Height,Color:Integer); inline;
var
 CurrentX:Integer;
 CurrentY:Integer;
 PixelOffset:Cardinal;
begin
 {}
 {Calculate the pixel's byte offset inside the buffer}
 PixelOffset:=(CurrentPage * PageSize) + X + (Y * FramebufferProperties.Pitch);
 
 {Draw each row of pixels}
 for CurrentY:=0 to Height - 1 do
  begin
   {Draw the pixel for each column}
   for CurrentX:=0 to Width - 1 do
    begin
     PByte(BufferStart + PixelOffset)^:=Color;
     
     {Move to the next pixel}
     Inc(PixelOffset);
    end;
   
   {Move to the next row}
   Inc(PixelOffset,FramebufferProperties.Pitch - Width);
  end;
end;*)


{Helper function to draw a rectangle in given color}
procedure FillRect(X,Y,Width,Height,Color:Integer); inline;
var
 CurrentY:Integer;
 PixelOffset:Cardinal;
begin
 {}
 {Calculate the byte offset of the top left}
 PixelOffset:=(CurrentPage * PageSize) + X + (Y * FramebufferProperties.Pitch);
 
 {Draw each row of pixels}
 for CurrentY:=0 to Height - 1 do
  begin
   FillChar(Pointer(BufferStart + PixelOffset)^,Width,Color);
   
   {Move to the next row}
   Inc(PixelOffset,FramebufferProperties.Pitch);
  end;
end;


{Helper function to clear the entire screen in one color}
procedure ClearScreen(Color:Integer); inline;
begin
 {}
 FillChar(Pointer(BufferStart + (CurrentPage * PageSize))^,PageSize,Color);
end;
 
 
{This is the animation drawing function, if you compare with the original C source you
 might notice that all of the variable names of been updated to be more readable so
 instead of DX we have DirectionX for example. This doesn't make any difference to
 the speed, size or memory use of the application because it is all removed by
 Free Pascal during the compile process but it does make it easier to read.}
procedure Draw;
var
 Frame:Integer;
 X:Integer;
 Y:Integer;
 Width:Integer;
 Height:Integer;
 DirectionX:Integer;
 DirectionY:Integer;
 
 StartTime:Int64;
 CompleteTime:Int64;
 TimeDifference:Int64;

 Element:Integer;
 
 FPS:Integer;
 Seconds:Integer;
 
 OffsetX:Integer;
 OffsetY:Integer;
begin
 {}
 {Intialize the random number generator}
 Randomize;
 
 {Setup the rectangle dimensions, actually they are a square but you could set Width and
  Height to be different. There are lots of values you can experiment with in this function}
 Width:=FramebufferProperties.PhysicalHeight div 10;
 Height:=Width;
 
 {Setup the starting position of each box, the first part sets the X and Y coordinates and
  the second part sets the direction and speed of the movement. Try changing the mod 10 to
  a higher or lower value to see what happens}
 for Element:=0 to NUM_ELEMENTS - 1 do
  begin
   ElementX[Element]:=Random(RANDOM_MAX) mod (FramebufferProperties.PhysicalWidth - Width); 
   ElementY[Element]:=Random(RANDOM_MAX) mod (FramebufferProperties.PhysicalHeight - Height);
   
   ElementDirX[Element]:=(Random(RANDOM_MAX) mod 10) + 1; 
   ElementDirY[Element]:=(Random(RANDOM_MAX) mod 10) + 1;
  end;
 
 {Setup the frames and seconds, actually we are just setting the total number of frames to be
  drawn but since we know if will draw 60 frames per second then we can confidently predict
  how many seconds the animation will last}
 FPS:=60;
 Seconds:=60;
 
 {Get the starting time in milliseconds ticks from the RTL GetTickCount64 function}
 StartTime:=GetTickCount64;
 
 {Loop for the number of seconds times the number of frames per second}
 for Frame:=0 to (FPS * Seconds) - 1 do
  begin
   {Change page to draw to (between 0 and 1), while page 0 is on screen we will draw
    page 1 and while page 1 is on screen we will draw page 0. Doing it that way means
    you don't see any sign of the drawing happening, it just looks like the boxes are
    smoothly floating around the screen.
    
    This variable determines the Y offset to pass to the framebuffer device and also
    the offset from the start of our framebuffer memory}
   CurrentPage:=(CurrentPage + 1) mod 2;
   
   {Clear the entire page to black so we can draw the boxes. It might look like the
    boxes are moving but in actual fact we have to draw them completely for every
    frame of the animation. No wonder we need to optimize our functions carefully}
   ClearScreen(0);
   
   {Draw each element (or box) on the current page. For each one there is an X and Y
    value that determines the current position, there is also a DirectionX and a
    DirectionY which determines which direction the box is moving (Up/Down/Left/Right)
    and also how fast it is moving.
    
    The width and height of each box were set above to a static value, can you see 
    how you could make the boxes different sizes?}
   for Element:=0 to NUM_ELEMENTS - 1 do
    begin
     {Earlier we declared some arrays to store the position of each box and above
      we setup the starting positions. On each iteration of the loop we need to
      retrieve those values from the arrays so we can use them to draw the current
      box}
     X:=ElementX[Element];
     Y:=ElementY[Element];
     DirectionX:=ElementDirX[Element];
     DirectionY:=ElementDirY[Element];
    
     {Draw the bouncing box, if you looked at the original code you might notice
      that we have changed the value here for the Ultibo version. What would be
      the result if you changed it back to 15 or even 63 instead?}
     FillRect(X,Y,Width,Height,(Element mod 127) + 1); 
     
     {We've drawn the box so move ot ready for the next frame. This is simply a
      matter of adding the direction X and Y values to the current position}
     X:=X + DirectionX;
     Y:=Y + DirectionY;
     
     {Check for the left and right sides of the screen. We want our boxes to
      'bounce' whenever they hit the side of the screen and go the other way.
      
      Here we check for the X position being either less than 0 which is the
      left side of the screen or greater than the screen width minus the box
      width which will be the right side of the screen.
      
      If we reach either of those we invert the direction value so our box
      moves in the opposite direction. Notice that the direction values are
      signed integers so they an be either positive or negative}
     if (X < 0) or (X > (FramebufferProperties.PhysicalWidth - Width)) then
      begin
       DirectionX:=-DirectionX; {Reverse the direction by inverting the value}
       X:=X + (2 * DirectionX); {Counteract the move we already did by adding double the new value}
      end;
      
     {Just like the sides we also check for the top and bottom of the screen}
     if (Y < 0) or (Y > (FramebufferProperties.PhysicalHeight - Height)) then
      begin
       DirectionY:=-DirectionY;
       Y:=Y + (2 * DirectionY);
      end;
     
     {Save the new position of the current box back to the arrays for the next frame}
     ElementX[Element]:=X;
     ElementY[Element]:=Y;
     ElementDirX[Element]:=DirectionX;
     ElementDirY[Element]:=DirectionY;
    end;
   
   {After drawing our frame we need to make sure that all of our pixels have
    actually been written back to memory by the CPU. 
    
    Since the Raspberry Pi uses both Level 1 and Level 2 caching in order to
    improve the performance it may have only written our pixels to cache so 
    far and not to memory.
    
    Because the graphics processor (GPU) is separate from the CPU it cannot
    see into the CPU cache so we need to clean the cache to make the CPU
    write everything to memory. The framebuffer properties gives us a flag
    to check that says if the memory is cached and if we need to do cleaning}
   if (FramebufferProperties.Flags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
    begin
     {If the flag is set we call CleanDataCacheRange() and pass the address and
      size of the current page. This is the fastest way to clean just the area that
      matters to us for the animation}
     CleanDataCacheRange(PtrUInt(BufferStart) + (CurrentPage * PageSize),PageSize);
    end; 
   
   {Switch to the new page by setting the Y offset of the framebuffer device, this
    will cause our new page to appear and then we can start all over again on the
    other page}
   OffsetX:=0;
   OffsetY:=CurrentPage * FramebufferProperties.PhysicalHeight;
   FramebufferDeviceSetOffset(FramebufferDevice,OffsetX,OffsetY,True);
   
   {Because the actual video hardware only refreshes the screen 60 times per second
    (or 60 FPS) then even though we have changed the Y offset above if we start
    drawing on the next page immediately it may still be showing on screen.
    
    This would produce a tearing effect of horizontal lines on the screen which
    doesn't look good for our smooth animation.
    
    In order to handle this we check the framebuffer properties to see if the
    device supports waiting for vertical sync (the time between each frame)
    before we proceed. If it doesn't support this option then we do the best we
    can and wait for an approximate amount of time}
   if (FramebufferProperties.Flags and FRAMEBUFFER_FLAG_SYNC) <> 0 then
    begin
     FramebufferDeviceWaitSync(FramebufferDevice);
    end
   else
    begin
     MicrosecondDelay(1000000 div FPS);
    end;    
  end;
 
 {Get the completion time in milliseconds ticks}
 CompleteTime:=GetTickCount64;
 
 {Work out the number of milliseconds between the start and end and display some information}
 TimeDifference:=CompleteTime - StartTime;
 ConsoleWriteLn('Completed ' + IntToStr(FPS * Seconds) + ' frames of ' + IntToStr(NUM_ELEMENTS) + ' elements in ' + IntToStr(TimeDifference div 1000) + 's ' + IntToStr(TimeDifference mod 1000) + 'ms');
 ConsoleWriteLn('Frame rate ' + IntToStr((FPS * Seconds) div (TimeDifference div 1000)) + ' frames per second');
end;
 
 
begin
 {This is the starting point of our program, first of all we need to do some setup}
 
 {Get the framebuffer device, we could use any device but most likely there is only
  one available so we just ask for the default device}
 FramebufferDevice:=FramebufferDeviceGetDefault;
 if FramebufferDevice <> nil then
  begin
   {Request the framebuffer properties which will tell us the size, depth and all
    sorts of other information about the framebuffer device}
   FramebufferDeviceGetProperties(FramebufferDevice,@FramebufferProperties);
   
   {Release the current framebuffer so we can setup a new one with different settings}
   FramebufferDeviceRelease(FramebufferDevice);

   {Wait for second to allow any messages generated by releasing the framebuffer to
    propogate through the system. To do this properly we really should tell the console
    not to attach to the framebuffer during boot, one of the many details needed when 
    creating real world applications}
   Sleep(1000);
   
   {Now we can adjust the framebuffer properties so that the color depth is 8 bits per
    pixel and the virtual height is twice the physical height which will give us two
    pages to draw our animation on. Again if we were doing this in a real application
    it would be best to check the flags first to see if the framebuffer device even
    supports virtual width and height}
   FramebufferProperties.Depth:=8;
   {The original source changes the width and height to 960 x 540 but there doesn't seem
    to be any reason to do this. The animation works at any size but you can try out the
    different values yourself for fun}
   //if (FramebufferProperties.PhysicalWidth > 960) or (FramebufferProperties.PhysicalHeight > 540) then
   // begin
   //  FramebufferProperties.PhysicalWidth:=960;
   //  FramebufferProperties.PhysicalHeight:=540;
   // end; 
   FramebufferProperties.VirtualWidth:=FramebufferProperties.PhysicalWidth;
   FramebufferProperties.VirtualHeight:=FramebufferProperties.PhysicalHeight * 2;
   
   {Pass the modified properties to the allocate function to allocate a new
    framebuffer with our changes enabled. Checking the return of this function
    would tell you if it was successful or not}
   FramebufferDeviceAllocate(FramebufferDevice,@FramebufferProperties);

   {Wait again just to be safe}
   Sleep(1000);
   
   {Because we set 8 bit color for the framebuffer to do this example we also
    need to set a palette. The framebuffer device or the driver might provide 
    a default palette but the simplest option is to set the one we want.
    
    Here we pass a prebuilt structure that contains the VGA Mode 13h color
    palette (or at least one version of it)}
   FramebufferDeviceSetPalette(FramebufferDevice,@VGAPalette);
   
   {We need to get the framebuffer properties again because we want to know the
    address of the framebuffer memory and also the length of each line in bytes
    which is known as the pitch}
   FramebufferDeviceGetProperties(FramebufferDevice,@FramebufferProperties);
   
   {From the properties work out the framebuffer variables}
   BufferStart:=Pointer(FramebufferProperties.Address);
   PageSize:=FramebufferProperties.Pitch * FramebufferProperties.PhysicalHeight;
   CurrentPage:=0;
  end;
   
 {Create a full screen console window (So we can output some information later)}
 ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULLSCREEN,True);
 
 {Go ahead and draw the animation}
 Draw;
 
 {Halt this thread when the drawing is done}
 ThreadHalt(0);
end.
