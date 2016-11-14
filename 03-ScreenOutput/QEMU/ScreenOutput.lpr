program ScreenOutput;

{$mode objfpc}{$H+}

{ Example 03 Screen Output                                                     }
{                                                                              }
{  This example builds on the previous ones by demonstrating some of the console}
{  functions available in Ultibo and how to use them to manipulate text on the }
{  screen.                                                                     }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{                                                                              }
{  QEMU VersatilePB version                                                    }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  QEMUVersatilePB,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  SysUtils;

{We'll need a few more variables for this example.}
var
 RowCount:LongWord;
 ColumnCount:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
 Handle1:TWindowHandle;
 Handle2:TWindowHandle;


begin
 {Let's create a console window again but this time on the left side of the screen}
 Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);

 {To prove that worked let's output some text on the console window}
 ConsoleWindowWriteLn(Handle1,'Welcome to Example 03 Screen Output');

 {So that things happen in a time frame we can see, let's wait about 3 seconds}
 ThreadSleep(3 * 1000);

 {Now let's get the current position of the console cursor into a couple of variables}
 ConsoleWindowGetXY(Handle1,CurrentX,CurrentY);

 {And we'll display those values on the screen}
 ConsoleWindowWriteLn(Handle1,'CurrentX = ' + IntToStr(CurrentX));
 ConsoleWindowWriteLn(Handle1,'CurrentY = ' + IntToStr(CurrentY));

 {Wait another 3 seconds so we can see that}
 ThreadSleep(3 * 1000);

 {Let's find out how big our console window is}
 ColumnCount:=ConsoleWindowGetCols(Handle1);
 RowCount:=ConsoleWindowGetRows(Handle1);

 {And print that on the screen as well}
 ConsoleWindowWriteLn(Handle1,'ColumnCount = ' + IntToStr(ColumnCount) + ' RowCount = ' + IntToStr(RowCount));

 {Wait 3 seconds again so we can see that}
 ThreadSleep(3 * 1000);

 {Now let's create another console window on the right side of the screen, notice
  that we use a different variable for the handle so we can still access the first
  console window.}
 Handle2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,True);

 {Update our original console}
 ConsoleWindowWriteLn(Handle1,'Printing some colored text on the new console');

 {Using some more of the console function we can print to the screen using
  ConsoleWindowWriteLnEx() which allows us to control the color of the text
  and background as well as where to output the text}
 CurrentX:=ConsoleWindowGetX(Handle2);
 CurrentY:=ConsoleWindowGetY(Handle2);
 ConsoleWindowWriteLnEx(Handle2,'This is some text in red',CurrentX,CurrentY,COLOR_RED,ConsoleWindowGetBackcolor(Handle2));

 {ConsoleWindowWriteLnEx() doesn't update the position of X and Y for us, we
  need to move the to the next row so we can write the next line underneath}
 Inc(CurrentY);
 ConsoleWindowSetY(Handle2,CurrentY);
 ConsoleWindowWriteLnEx(Handle2,'This is some text in green',CurrentX,CurrentY,COLOR_GREEN,ConsoleWindowGetBackcolor(Handle2));

 {And one more time in yellow}
 Inc(CurrentY);
 ConsoleWindowSetY(Handle2,CurrentY);
 ConsoleWindowWriteLnEx(Handle2,'This is some text in yellow',CurrentX,CurrentY,COLOR_YELLOW,ConsoleWindowGetBackcolor(Handle2));

 {Wait a few more seconds so we can see that}
 ThreadSleep(5 * 1000);

 {Update our original console}
 ConsoleWindowWriteLn(Handle1,'Printing some text at the bottom of the new console');

 {Wait about some text at the bottom of the screen instead, we'll use ConsoleWindowWriteEx()
  instead so it doesn't scroll the screen up}
 ConsoleWindowWriteEx(Handle2,'This text should be in the last row of the screen',CurrentX,ConsoleWindowGetMaxY(Handle2),ConsoleWindowGetForecolor(Handle2),ConsoleWindowGetBackcolor(Handle2));

 {Wait a bit more}
 ThreadSleep(5 * 1000);

 {Update our original console}
 ConsoleWindowWriteLn(Handle1,'Clearing the new console');

 {Finally how about we clear the console window to get rid of all of that}
 ConsoleWindowClear(Handle2);

 {And say goodbye}
 ConsoleWindowWriteLn(Handle1,'All done, thanks for watching');

 {We're not doing a loop this time so we better halt this thread before it exits}
 ThreadHalt(0);
end.

