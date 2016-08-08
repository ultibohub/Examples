unit MainUnit;

{$mode delphi}{$H+}

{ A typical main unit containing code to start the program and handle the main }
{ interfaces like the console.                                                 }

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  HTTP,
  WebStatus,
  Font,
  Console,
  ConsoleShell,
  GraphicsConsole,
  SensorUnit,
  GraphUnit,
  TimeUnit;


const
 {Define the ADUG green used on the website, the Raspberry Pi includes a full HD 32bit per pixel framebuffer}
 ADUG_GREEN = $FF336667;

 
var
 MainWindow:THandle;
 SensorWindow:THandle;
  
  
function SensormaticStart:Boolean;

  
implementation


{A function to display the main "Semsormatic" window}
procedure DisplayMainWindow;
var
 Title:String;
 Text:String;
 Top:LongWord;
 Left:LongWord;
 Width:LongWord;
 Height:LongWord;
 LargeFont:THandle;
 SmallFont:THandle;
 ConsoleDevice:PConsoleDevice;
begin
 {Get some fonts and the console device}
 LargeFont:=FontFindByName('Bitstream_Vera_Sans_Mono_40');
 SmallFont:=FontFindByName('Sun-12x22');
 ConsoleDevice:=ConsoleDeviceGetDefault;
 
 {Create our main window}
 MainWindow:=GraphicsWindowCreate(ConsoleDevice,CONSOLE_POSITION_TOPLEFT);
 
 {Check the window size}
 if GraphicsWindowGetWidth(MainWindow) < 900 then
  begin
   LargeFont:=FontFindByName('Bitstream_Vera_Sans_Mono_25');
  end;
  
 {Setup some defaults}
 GraphicsWindowSetForecolor(MainWindow,COLOR_WHITE);
 GraphicsWindowSetBackcolor(MainWindow,ADUG_GREEN); 
 GraphicsWindowSetFont(MainWindow,LargeFont);
 GraphicsWindowClear(MainWindow);
 
 {Display our title}
 Title:='Ultibo Sensormatic';
 Width:=GraphicsWindowGetWidth(MainWindow);
 Height:=GraphicsWindowGetHeight(MainWindow);
 Left:=(Width - (Length(Title) * FontGetWidth(LargeFont))) div 2;
 Top:=(Height - (4 * FontGetHeight(LargeFont))) div 2;
 
 GraphicsWindowDrawText(MainWindow,Title,Left,Top);
 
 {Say hi to our friends at ADUG}
 Text:='ADUG Symposium 2016';
 Width:=GraphicsWindowGetWidth(MainWindow);
 Height:=GraphicsWindowGetHeight(MainWindow);
 Left:=(Width - (Length(Text) * FontGetWidth(LargeFont))) div 2;
 Top:=(Height - FontGetHeight(LargeFont)) div 2;
 
 GraphicsWindowDrawText(MainWindow,Text,Left,Top);
 
 {Add some info so we know where we are!}
 Inc(Top,FontGetHeight(LargeFont));
 GraphicsWindowDrawTextEx(MainWindow,SmallFont,'Sydney, August 4th',Left,Top,COLOR_SILVER,ADUG_GREEN); 

 Inc(Top,FontGetHeight(SmallFont));
 GraphicsWindowDrawTextEx(MainWindow,SmallFont,'Melbourne, August 5th',Left,Top,COLOR_SILVER,ADUG_GREEN); 
 
end;


{A function to display our console shell}
procedure DisplayShellWindow;
var
 ConsoleDevice:PConsoleDevice;
begin
 {Get our default console device}
 ConsoleDevice:=ConsoleDeviceGetDefault;
 
 {Override some default values}
 WINDOW_DEFAULT_FORECOLOR:=COLOR_WHITE;
 WINDOW_DEFAULT_BACKCOLOR:=COLOR_BLACK;
 CONSOLE_SHELL_POSITION:=CONSOLE_POSITION_BOTTOMLEFT;
 
 {Force creation of a new shell window}
 ConsoleShellDeviceAdd(ConsoleDevice,True);
 
end;


{Our startup function, called from the project file}
function SensormaticStart:Boolean;
var
 Graph:TGraphPage;
 Folder:THTTPFolder;
 Listener:THTTPListener;
begin
 Result:=True;
 
 {Display our windows}
 DisplayMainWindow;
 
 DisplayShellWindow;
 
 {Create our threads}
 StartSensorThread;
 
 StartGraphThread;
 
 StartTimeThread;
 
 {Create our web interface}
 {An instance of the THTTPListener class}
 Listener:=THTTPListener.Create;
 Listener.Active:=True;
 
 {An instance of our custom graph page from the graph unit}
 Graph:=TGraphPage.Create(SensorData);
 
 {Register our custom graph page with our listener}
 Listener.RegisterDocument('',Graph);
 
 {Register the web status unit with our listener}
 WebStatusRegister(Listener,'','',False);
 
 {Create an HTTPFolder object to hold non dynamic content like the JavaScript classes}
 Folder:=THTTPFolder.Create;
 Folder.Name:='/'; 
 Folder.Folder:='C:\www';
 Folder.IndexPage:='index.htm';
 Folder.AllowCache:=True;
 Folder.AllowListing:=True;
 Folder.AllowSubtree:=True;

 {And register the folder with our listener}
 Listener.RegisterDocument('',Folder);
 
end;


end.

