unit TimeUnit;

{$mode delphi}{$H+}

{ A simple thread unit which just updates the displayed time and uptime on our }
{ console screen. We could have used the main thread for this but this unit    }
{ also shows how to create a thread without using the TThread class.           }

interface

uses
  GlobalConfig,
  GlobalConst,
  Platform,
  Threads,
  SysUtils,
  Ultibo,
  Font,
  Console,
  GraphicsConsole;

  
procedure StartTimeThread;
  
  
implementation


uses MainUnit;


{The "main" function for our time thread}
function ExecuteTimeThread(Data:Pointer):PtrInt;
var
 Text:String;
 Font:THandle;
 Top:LongWord;
 Left:LongWord;
 Width:LongWord;
 Height:LongWord;
 DateTime:TDateTime;
begin
 Result:=0;
 
 {Set the name so we can find it in the threads list}
 ThreadSetName(ThreadGetCurrent,'Time Thread');
 
 {Setup the font and get width and height}
 Font:=FontFindByName('Sun-12x22');
 Width:=GraphicsWindowGetWidth(MainWindow);
 Height:=GraphicsWindowGetHeight(MainWindow);
 
 {Loop checking and setting the time}
 while True do
  begin
   {Get the uptime}
   DateTime:=SystemFileTimeToDateTime(Uptime);
   
   {Check the Width}
   if Width < 700 then
    begin
     {And the time to be displayed}
     Text:='Time: '  + DateTimeToStr(Now);
     
     {Calculate our position in the main window}
     Left:=Width - ((Length(Text) + 1) * FontGetWidth(Font));
     Top:=Height - ((2 * FontGetHeight(Font)) + 1);
     
     {Output the time}
     GraphicsWindowDrawTextEx(MainWindow,Font,Text,Left,Top,COLOR_SILVER,ADUG_GREEN);
     
     {Get the uptime to display}
     Text:='Uptime: ' + IntToStr(Trunc(DateTime)) + ' days ' + TimeToStr(DateTime);
     
     {Calculate our position}
     Left:=Width - ((Length(Text) + 1) * FontGetWidth(Font));
     Top:=Height - (FontGetHeight(Font) + 1);
     
     {Output the uptime}
     GraphicsWindowDrawTextEx(MainWindow,Font,Text,Left,Top,COLOR_SILVER,ADUG_GREEN);
    end
   else
    begin   
     {And the text to be displayed}
     Text:='Time: '  + DateTimeToStr(Now) + ' / Uptime: ' + IntToStr(Trunc(DateTime)) + ' days ' + TimeToStr(DateTime);
     
     {Calculate our position in the main window}
     Left:=Width - ((Length(Text) + 1) * FontGetWidth(Font));
     Top:=Height - (FontGetHeight(Font) + 1);
     
     {And output the text}
     GraphicsWindowDrawTextEx(MainWindow,Font,Text,Left,Top,COLOR_SILVER,ADUG_GREEN);
    end; 
   
   {Go back to sleep}
   Sleep(1000);
  end;
  
end;


{Simple wrapper function to start our time thread}
procedure StartTimeThread;
var
 ThreadHandle:THandle;
begin
 {This one uses BeginThread instead of the TThread class}
 BeginThread(ExecuteTimeThread,nil,ThreadHandle,THREAD_STACK_DEFAULT_SIZE);
 
end;


end.

