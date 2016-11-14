program TextEditor;

{ Example 17 Text Editor                                                       }
{                                                                              }
{ This example uses the FreeVision text mode windowing package to create a     }
{ simple text editor that supports multiple windows, cut, copy and paste as    }
{ well as file open and save options                                           }
{                                                                              }
{ The example is based completely on the testapp example included with FPC and }
{ is intended to show both the features of FreeVision and test many of the     }
{ standard operations.                                                         }
{                                                                              }
{ FreeVision is an open source clone of the popular Turbo Vision library from  }
{ the early versions of Turbo Pascal and Turbo C. Under Ultibo all functions   }
{ are supported and other than some minor details everything is working the    }
{ same as it does for any other platform.                                      }
{                                                                              }
{ If you want to know more about creating applications using FreeVision see    }
{ the Free Pascal wiki or many other online sources of information.            }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 3B version                                                     }
{   What's the difference? See Project, Project Options, Config and Target.    }


{Include the platform.inc file from FreeVision which sets up the standard options}
{$I platform.inc}

{Declare some units used by this example.}
uses
 RaspberryPi3, {The RaspberryPi3 unit gives us all the relevant drivers}
 GlobalConfig,
 GlobalConst,
 Platform,
 Threads,
 Console,
 SysUtils,
 
 {The main FreeVision units for App, Views, Drivers etc}
 Objects,
 Drivers,
 Views,
 Editors,
 Menus,
 Dialogs,
 App,
 {Extra FreeVision units for dialogs and gadgets}
 FVConsts,
 AsciiTab,
 Gadgets,
 TimedDlg,
 MsgBox,
 StdDlg;
 
{Some constants used by the TVDemo object} 
const
 cmAppToolbar = 1000;
 cmWindow1    = 1001;
 cmWindow2    = 1002;
 cmWindow3    = 1003;
 cmTimedBox   = 1004;
 cmAscii      = 1010;
 cmCloseWindow1    = 1101;
 cmCloseWindow2    = 1102;
 cmCloseWindow3    = 1103;
 
{---------------------------------------------------------------------------}
{           TTVDemo OBJECT - STANDARD APPLICATION WITH MENU                 }
{---------------------------------------------------------------------------}
type
 PTVDemo = ^TTVDemo;

 { TTVDemo }
 TTVDemo = object (TApplication)
      ClipboardWindow: PEditWindow;
      Clock: PClockView;
      Heap: PHeapView;
      P1,P2,P3 : PGroup;
      ASCIIChart : PAsciiChart;
    constructor Init;
    procedure Idle; virtual;
    procedure HandleEvent(var Event : TEvent);virtual;
    procedure InitMenuBar; virtual;
    procedure InitDeskTop; virtual;
    procedure InitStatusLine; virtual;
    procedure Window1;
    procedure Window2;
    procedure Window3;
    procedure TimedBox;
    procedure AsciiWindow;
    procedure ShowAboutBox;
    procedure NewEditWindow;
    procedure OpenFile;
    procedure CloseWindow(var P : PGroup);
  end;
 
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TTvDemo OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
 
constructor TTvDemo.Init;
var
 R: TRect;
begin
 EditorDialog := @StdEditorDialog;
 inherited Init;
 { Initialize demo gadgets }

 GetExtent(R);
 R.A.X := R.B.X - 9; R.B.Y := R.A.Y + 1;
 Clock := New(PClockView, Init(R));
 Insert(Clock);

 GetExtent(R);
 ClipboardWindow := New(PEditWindow, Init(R, '', wnNoNumber));
 if ValidView(ClipboardWindow) <> nil then
 begin
   ClipboardWindow^.Hide;
   ClipboardWindow^.Editor^.CanUndo := False;
   InsertWindow(ClipboardWindow);
   Clipboard := ClipboardWindow^.Editor;
 end;
end;
 
procedure TTVDemo.Idle;

 function IsTileable(P: PView): Boolean;
 begin
   IsTileable := (P^.Options and ofTileable <> 0) and
     (P^.State and sfVisible <> 0);
 end;
 
begin
 inherited Idle;
 Clock^.Update;
 Heap^.Update;
 if Desktop^.FirstThat(@IsTileable) <> nil then
   EnableCommands([cmTile, cmCascade])
 else
   DisableCommands([cmTile, cmCascade]);
end;
 
procedure TTVDemo.HandleEvent(var Event : TEvent);
begin
 inherited HandleEvent(Event);                      { Call ancestor }
 if (Event.What = evCommand) then begin
   case Event.Command of
     cmClipBoard:
       begin
         ClipboardWindow^.Select;
         ClipboardWindow^.Show;
       end;
     cmNew     : NewEditWindow;
     cmOpen    : OpenFile;
     cmWindow1 : Window1;
     cmWindow2 : Window2;
     cmWindow3 : Window3;
     cmTimedBox: TimedBox;
     cmAscii   : AsciiWindow;
     cmCloseWindow1 : CloseWindow(P1);
     cmCloseWindow2 : CloseWindow(P2);
     cmCloseWindow3 : CloseWindow(P3);
     cmAbout: ShowAboutBox;
     else Exit;                                     { Unhandled exit }
   end;
 end;
 ClearEvent(Event);
end;
 
procedure TTVDemo.InitMenuBar;
var
 R: TRect;
begin
 GetExtent(R);                                      { Get view extents }
 R.B.Y := R.A.Y + 1;                                { One line high  }
 MenuBar := New(PMenuBar, Init(R, NewMenu(
  NewSubMenu('~F~ile', 0, NewMenu(
    StdFileMenuItems(Nil)),                         { Standard file menu }
  NewSubMenu('~E~dit', 0, NewMenu(
    StdEditMenuItems(
    NewLine(
    NewItem('~V~iew Clipboard', '', kbNoKey, cmClipboard, hcNoContext,
    nil)))),                 { Standard edit menu plus view clipboard}
  NewSubMenu('~T~est', 0, NewMenu(
    NewItem('~A~scii Chart','',kbNoKey,cmAscii,hcNoContext,
    NewItem('Window ~1~','',kbNoKey,cmWindow1,hcNoContext,
    NewItem('Window ~2~','',kbNoKey,cmWindow2,hcNoContext,
    NewItem('Window ~3~','',kbNoKey,cmWindow3,hcNoContext,
    NewItem('~T~imed Box','',kbNoKey,cmTimedBox,hcNoContext,
    NewItem('Close Window 1','',kbNoKey,cmCloseWindow1,hcNoContext,
    NewItem('Close Window 2','',kbNoKey,cmCloseWindow2,hcNoContext,
    NewItem('Close Window 3','',kbNoKey,cmCloseWindow3,hcNoContext,
    Nil))))))))),
  NewSubMenu('~W~indow', 0, NewMenu(
    StdWindowMenuItems(Nil)),        { Standard window  menu }
  NewSubMenu('~H~elp', hcNoContext, NewMenu(
    NewItem('~A~bout...','',kbNoKey,cmAbout,hcNoContext,
    nil)),
  nil))))) //end NewSubMenus
 ))); //end MenuBar
end;
 
procedure TTvDemo.InitDesktop;
var
 R: TRect; {ToolBar: PToolBar;}
begin
 GetExtent(R);          { Get app extents }
 Inc(R.A.Y);            { Adjust top down }
 Dec(R.B.Y);            { Adjust bottom up }
(*   ToolBar := New(PToolBar, Init(R.A.X*FontWidth,
     R.A.Y*FontHeight, (R.B.X-R.A.X)*FontWidth, 20,
     cmAppToolBar));
   If (ToolBar <> Nil) Then Begin
     R.A.X := R.A.X*FontWidth;
     R.A.Y := R.A.Y*FontHeight + 25;
     R.B.X := -R.B.X*FontWidth;
     R.B.Y := -R.B.Y*Fontheight;
     ToolBar^.AddTool(NewToolEntry(cmQuit, True,
       '20X20EXIT', 'ToolBar.Res'));
     ToolBar^.AddTool(NewToolEntry(cmNew, True,
       '20X20NEW', 'ToolBar.Res'));
     ToolBar^.AddTool(NewToolEntry(cmOpen, True,
       '20X20LOAD', 'ToolBar.Res'));
     Insert(ToolBar);
   End;*)
 Desktop := New(PDeskTop, Init(R));
end;
 
procedure TTVDemo.InitStatusLine;
var
 R: TRect;
begin
 GetExtent(R);
 R.A.Y := R.B.Y - 1;
 R.B.X := R.B.X - 12;
 New(StatusLine,
   Init(R,
     NewStatusDef(0, $EFFF,
       NewStatusKey('~F3~ Open', kbF3, cmOpen,
       NewStatusKey('~F4~ New', kbF4, cmNew,
       NewStatusKey('~Alt+F3~ Close', kbAltF3, cmClose,
       StdStatusKeys(nil
       )))),nil
     )
   )
 );
 
 GetExtent(R);
 R.A.X := R.B.X - 12; R.A.Y := R.B.Y - 1;
 Heap := New(PHeapView, Init(R));
 Insert(Heap);
end;
 
procedure TTvDemo.Window1;
var
 R: TRect; P: PGroup;
begin
 { Create a basic window with static text and radio }
 { buttons. The buttons should be orange and white  }
 R.Assign(5, 1, 35, 16);                            { Assign area }
 P := New(PWindow, Init(R, 'TEST WINDOW 1', 1));    { Create a window }
 If (P <> Nil) Then begin                           { Window valid }
   R.Assign(5, 5, 20, 6);                           { Assign area }
   P^.Insert(New(PInputLine, Init(R, 30)));
   R.Assign(5, 8, 20, 9);                           { Assign area }
   P^.Insert(New(PRadioButtons, Init(R,
     NewSItem('Test',
     NewSITem('Item 2', Nil)))));                   { Red radio button }
   R.Assign(5, 10, 28, 11);                         { Assign area }
   P^.Insert(New(PStaticText, Init(R,
     'SOME STATIC TEXT')));                         { Insert static text }
 end;
 Desktop^.Insert(P);                                { Insert into desktop }
 P1:=P;
end;

procedure TTvDemo.AsciiWindow;
begin
 if ASCIIChart=nil then
   begin
     New(ASCIIChart, Init);
     Desktop^.Insert(ASCIIChart);
   end
 else
   ASCIIChart^.Focus;
end;

procedure TTVDemo.ShowAboutBox;
begin
 MessageBox(#3'Free Vision TUI Framework'#13 +
   #3'Test/Demo Application'#13+
   #3'(www.freepascal.org)',
   nil, mfInformation or mfOKButton);
end;

procedure TTVDemo.NewEditWindow;
var
 R: TRect;
begin
 R.Assign(0, 0, 60, 20);
 InsertWindow(New(PEditWindow, Init(R, '', wnNoNumber)));
end;

procedure TTVDemo.OpenFile;
var
 R: TRect;
 FileDialog: PFileDialog;
 FileName: FNameStr;
const
 FDOptions: Word = fdOKButton or fdOpenButton;
begin
 FileName := '*.*';
 New(FileDialog, Init(FileName, 'Open file', '~F~ile name', FDOptions, 1));
 if ExecuteDialog(FileDialog, @FileName) <> cmCancel then
 begin
   R.Assign(0, 0, 75, 20);
   InsertWindow(New(PEditWindow, Init(R, FileName, wnNoNumber)));
 end;
end;

procedure TTvDemo.TimedBox;
var
 X: longint;
 S: string;
begin
 X := TimedMessageBox ('Everything OK?', nil, mfConfirmation or mfOKCancel, 10);
 case X of
  cmCancel: MessageBox ('cmCancel', nil, mfOKButton);
  cmOK: MessageBox ('cmOK', nil, mfOKButton);
 else
  begin
   Str (X, S);
   MessageBox (S, nil, mfOKButton);
  end;
 end;
end;

procedure TTvDemo.CloseWindow(var P : PGroup);
begin
 If Assigned(P) then
   begin
     Desktop^.Delete(P);
     Dispose(P,Done);
     P:=Nil;
   end;
end;

procedure TTvDemo.Window2;
var
 R: TRect; P: PGroup;
begin
 { Create a basic window with check boxes. The  }
 { check boxes should be orange and white       }
 R.Assign(15, 3, 45, 18);                           { Assign area }
 P := New(PWindow, Init(R, 'TEST WINDOW 2', 2));    { Create window 2 }
 If (P <> Nil) Then begin                           { Window valid }
   R.Assign(5, 5, 20, 7);                           { Assign area }
   P^.Insert(New(PCheckBoxes, Init(R,
     NewSItem('Test check',
     NewSITem('Item 2', Nil)))));                   { Create check box }
 end;
 Desktop^.Insert(P);                                { Insert into desktop }
 P2:=P;
end;

procedure TTvDemo.Window3;
var
 R: TRect; P: PGroup; B: PScrollBar;
 List: PStrCollection; Lb: PListBox;
begin
 { Create a basic dialog box. In it are buttons,  }
 { list boxes, scrollbars, inputlines, checkboxes }
 R.Assign(32, 2, 77, 18);                           { Assign screen area }
 P := New(PDialog, Init(R, 'TEST DIALOG'));         { Create dialog }
 If (P <> Nil) Then begin                           { Dialog valid }
   R.Assign(5, 5, 20, 7);                          { Allocate area }
   P^.Insert(New(PCheckBoxes, Init(R,
     NewSItem('Test',
     NewSITem('Item 2', Nil)))));                   { Insert check box }
   R.Assign(5, 2, 20, 3);                           { Assign area }
   B := New(PScrollBar, Init(R));                   { Insert scroll bar }
   If (B <> Nil) Then begin                         { Scrollbar valid }
     B^.SetRange(0, 100);                           { Set scrollbar range }
     B^.SetValue(50);                               { Set position }
     P^.Insert(B);                                  { Insert scrollbar }
   end;
   R.Assign(5, 10, 20, 11);                         { Assign area }
   P^.Insert(New(PInputLine, Init(R, 60)));         { Create input line }
   R.Assign(5, 13, 20, 14);                         { Assign area }
   P^.Insert(New(PInputLine, Init(R, 60)));         { Create input line }
   R.Assign(40, 8, 41, 14);                         { Assign area }
   B := New(PScrollBar, Init(R));                   { Create scrollbar }
   P^.Insert(B);                                    { Insert scrollbar }
   R.Assign(25, 8, 40, 14);                         { Assign area }
   Lb := New(PListBox, Init(R, 1, B));              { Create listbox }
   P^.Insert(Lb);                                   { Insert listbox }
   List := New(PStrCollection, Init(10, 5));        { Create string list }
   List^.AtInsert(0, NewStr('Zebra'));              { Insert text }
   List^.AtInsert(1, NewStr('Apple'));              { Insert text }
   List^.AtInsert(2, NewStr('Third'));              { Insert text }
   List^.AtInsert(3, NewStr('Peach'));              { Insert text }
   List^.AtInsert(4, NewStr('Rabbit'));             { Insert text }
   List^.AtInsert(5, NewStr('Item six'));           { Insert text }
   List^.AtInsert(6, NewStr('Jaguar'));             { Insert text }
   List^.AtInsert(7, NewStr('Melon'));              { Insert text }
   List^.AtInsert(8, NewStr('Ninth'));              { Insert text }
   List^.AtInsert(9, NewStr('Last item'));          { Insert text }
   Lb^.Newlist(List);                               { Give list to listbox }
   R.Assign(30, 2, 40, 4);                          { Assign area }
   P^.Insert(New(PButton, Init(R, '~O~k', 100, bfGrabFocus)));{ Create okay button }
   R.Assign(30, 15, 40, 17);                        { Assign area }
   Desktop^.Insert(P);                              { Insert dialog }
   P3:=P;
 end;
end;
 
var
 MyApp: TTvDemo;
 
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                             MAIN PROGRAM START                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
 
begin
 {We don't need to create a console window, the FreeVision library does all of the for us.
 
  There are some parameters in the GlobalConfig unit that control where and how the
  console window is created and we can change this before initializing our app to
  setup the behaviour the way we want it.}

 {We can control the position of the console window using CONSOLE_VIDEO_POSITION.
 
  It defaults to CONSOLE_POSITION_FULL but in this case we'll set it to CONSOLE_POSITION_LEFT
  instead so the app only occupies half of the window}
 CONSOLE_VIDEO_POSITION:=CONSOLE_POSITION_LEFT;
 
 {The CONSOLE_VIDEO_DEVICE variable allow us to specify a device other than the default
  console so our FreeVision app could appear on a small TFT screen or somewhere else}
 //CONSOLE_VIDEO_DEVICE:='';
 
 {If we already have a console window and want the FreeVision app to appear on it then
  we can just assign the Handle to the CONSOLE_VIDEO_WINDOW variable}
 //CONSOLE_VIDEO_WINDOW
 
 {FreeVision uses the Default8x16 font normally because that one contains the ASCII
  characters needed to draw the window elements on the screen. You can force it to
  use another font by setting the CONSOLE_VIDEO_FONT variable. Setting it to the
  Default8x9 font will allow a lot of text on the screen at once}
 //CONSOLE_VIDEO_FONT:='Default8x9';
 
 {Wait for C: drive to be ready}
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 
 {Intitialize our FreeVision app}
 MyApp.Init;                    
 
 {Run our app which will only return if an error occurs or we choose exit from the menu}
 MyApp.Run; 
 
 {Call Done to dispose of the objects and cleanup}
 MyApp.Done;
 
 {Halt the current thread if we exit from the FreeVision app} 
 ThreadHalt(0);
end.
