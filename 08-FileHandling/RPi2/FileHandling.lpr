program FileHandling;

{$mode objfpc}{$H+}

{ Example 08 File Handling                                                     }
{                                                                              }
{  This example demonstrates just a few basic functions of file handling which }
{  is a major topic in itself.                                                 }
{                                                                              }
{  For more information on all of the available file function see the Ultibo   }
{  Wiki or the Free Pascal user guide.                                         }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 2B version                                                     }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  BCM2836,
  SysUtils,
  Classes,     {Include the common classes}
  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC,         {Include the MMC/SD core to access our SD card}
  BCM2709;     {And also include the MMC/SD driver for the Raspberry Pi}

{A window handle plus a couple of others.}
var
 Count:Integer;
 Filename:String;
 SearchRec:TSearchRec;
 StringList:TStringList;
 FileStream:TFileStream;
 WindowHandle:TWindowHandle;


begin
 {Create our window}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Output the message}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 08 File Handling');
 ConsoleWindowWriteLn(WindowHandle,'');

 {We may need to wait a couple of seconds for any drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');

 {First let's list the contents of the SD card. We can guess that it will be C:\
  drive because we didn't include the USB host driver.}
 ConsoleWindowWriteLn(WindowHandle,'Contents of drive C:\');

 {To list the contents we need to use FindFirst/FindNext, start with FindFirst}
 if FindFirst('C:\*.*',faAnyFile,SearchRec) = 0 then
  begin
   {If FindFirst succeeds it will return 0 and we can proceed with the search}
   repeat
    {Print the file found to the screen}
    ConsoleWindowWriteLn(WindowHandle,'Filename is ' + SearchRec.Name + ' - Size is ' + IntToStr(SearchRec.Size) + ' - Time is ' + DateTimeToStr(FileDateToDateTime(SearchRec.Time)));

   {We keep calling FindNext until there are no more files to find}
   until FindNext(SearchRec) <> 0;
  end;

 {After any call to FindFirst, you must call FindClose or else memory will be leaked}
 FindClose(SearchRec);
 ConsoleWindowWriteLn(WindowHandle,'');

 {Let's try creating a file and writing some text to it, we'll assign our filename
  to a variable.}
 Filename:='C:\Example 08 File Handling.txt';

 {We should check if the file exists first before trying to create it}
 ConsoleWindowWriteLn(WindowHandle,'Checking to see if ' + Filename + ' exists');
 if FileExists(Filename) then
  begin
   {If it does exist we can delete it}
   ConsoleWindowWriteLn(WindowHandle,'Deleting the file ' + Filename);
   DeleteFile(Filename);
  end;

 {Now create the file, let's use a TFileStream class to do this. We pass both the
  filename and the mode to TFileStream. fmCreate tells it to create a new file.}
 ConsoleWindowWriteLn(WindowHandle,'Creating a new file ' + Filename);
 {TFileStream will raise an exception if creating the file fails}
 try
  FileStream:=TFileStream.Create(Filename,fmCreate);

  {We've created the file, now we need to write some content to it, we can use
   a TStringList for that but there are many other ways as well.}
  StringList:=TStringList.Create;

  {Add some text to our string list}
  StringList.Add('Example 08 File Handling');
  StringList.Add('This is a test file created by the example');
  StringList.Add('Here is a another line of text as well.');

  {Since TStringList has a SaveToStream method, we can just call that to write
   all the strings to our new file.}
  ConsoleWindowWriteLn(WindowHandle,'Saving the TStringList to the file');
  StringList.SaveToStream(FileStream);

  {With that done we can close the file and free the string list}
  ConsoleWindowWriteLn(WindowHandle,'Closing the file');
  ConsoleWindowWriteLn(WindowHandle,'');
  FileStream.Free;
  StringList.Free;

  {Did it work? Let's open the file and display it on screen to see.}
  ConsoleWindowWriteLn(WindowHandle,'Opening the file ' + Filename);
  try
   FileStream:=TFileStream.Create(Filename,fmOpenReadWrite);

   {Recreate our string list}
   StringList:=TStringList.Create;

   {And use LoadFromStream to read it}
   ConsoleWindowWriteLn(WindowHandle,'Loading the TStringList from the file');
   StringList.LoadFromStream(FileStream);

   {Iterate the strings and print them to the screen}
   ConsoleWindowWriteLn(WindowHandle,'The contents of the file are:');
   for Count:=0 to StringList.Count - 1 do
    begin
     ConsoleWindowWriteLn(WindowHandle,StringList.Strings[Count]);
    end;

   {Close the file and free the string list again}
   ConsoleWindowWriteLn(WindowHandle,'Closing the file');
   ConsoleWindowWriteLn(WindowHandle,'');
   FileStream.Free;
   StringList.Free;

   {If you remove the SD card and put in back in your computer, you should see the
    file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
    see the contents exactly as they appeared on screen.}
  except
   {TFileStream couldn't open the file}
   ConsoleWindowWriteLn(WindowHandle,'Failed to open the file ' + Filename);
  end;
 except
  {Something went wrong creating the file}
  ConsoleWindowWriteLn(WindowHandle,'Failed to create the file ' + Filename);
 end;

 {Halt the thread}
 ThreadHalt(0);
end.

