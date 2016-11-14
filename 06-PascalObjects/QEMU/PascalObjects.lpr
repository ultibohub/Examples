program PascalObjects;

{$mode objfpc}{$H+}

{ Example 06 Pascal Objects                                                    }
{                                                                              }
{  This example demonstrates some of the object pascal classes available in the}
{  standard run time library.                                                  }
{                                                                              }
{  There are a huge variety of classes and you can create your own very easily }
{                                                                              }
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
  SysUtils,
  Classes;  {The main classes unit for the Free Pascal RTL}

{A window handle and some class variables.}
var
 Count:Integer;
 StringList1:TStringList;
 StringList2:TStringList;
 MemoryStream:TMemoryStream;
 WindowHandle:TWindowHandle;


begin
 {Create our console window}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Output the welcome text}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 06 Pascal Classes');

 {A class you will see used regularly in Free Pascal is the TStringList

  The TStringList is an object that can hold a list of strings (or text).

  Let's create a TStringList and add some strings to it}
 StringList1:=TStringList.Create;
 StringList1.Add('This is the first string in the list');
 StringList1.Add('This is the second string in the list');
 StringList1.Add('Here is another string, that would be 3 now');
 StringList1.Add('And one more for good luck, must be 4 in total');

 {Now that we have added the strings to the list we can print them to the screen}
 ConsoleWindowWriteLn(WindowHandle,'Here are the strings in StringList1');

 {We use a loop to iterate through them, the strings each have an index which starts
 at zero and goes to the total number of strings minus one.}
 for Count:=0 to StringList1.Count - 1 do
  begin
   {Access each string using the Strings[] parameter and print it to the screen}
   ConsoleWindowWriteLn(WindowHandle,StringList1.Strings[Count]);
  end;
 ConsoleWindowWriteLn(WindowHandle,'');

 {We can use the Count parameter to tell how many strings there are in our list}
 ConsoleWindowWriteLn(WindowHandle,'There are ' + IntToStr(StringList1.Count) + ' strings in StringList1');
 ConsoleWindowWriteLn(WindowHandle,'');

 {Another very useful class is the TStream, it has many descendants one of them
  is the TMemoryStream which is memory based.

  A TStream can read and write data from one object to another and allows code to
  be written that is unaware of the underlying implementation of the stream. For
  example, writing data to a TMemoryStream uses exactly the same code as writing
  to a TFileStream and you don't need to consider if the data is going to memory
  or to a file because it just happens.

  Let's create a memory stream and copy our TStringList to it.}
 MemoryStream:=TMemoryStream.Create;

 {TStringList has a method called SaveToStream which we can use to copy the strings}
 StringList1.SaveToStream(MemoryStream);

 {We can print the size of the memory stream to see if it contains anything}
 ConsoleWindowWriteLn(WindowHandle,'The MemoryStream size is ' + IntToStr(MemoryStream.Size));
 ConsoleWindowWriteLn(WindowHandle,'');

 {Now, to prove that worked let's create another TStringList and copy the data
  back from the TMemoryStream.}
 StringList2:=TStringList.Create;

 {The opposite of SaveToStream is LoadFromStream so we use that to copy the strings
  to our new string list. First we need to reset the position of the stream to zero.}
 MemoryStream.Position:=0;
 StringList2.LoadFromStream(MemoryStream);

 {We can see the results by printing the strings of StringList2 to the screen.}
 ConsoleWindowWriteLn(WindowHandle,'Here are the strings in StringList2');
 for Count:=0 to StringList2.Count - 1 do
  begin
   {Access each string using the Strings[] parameter and print it to the screen}
   ConsoleWindowWriteLn(WindowHandle,StringList2.Strings[Count]);
  end;
 ConsoleWindowWriteLn(WindowHandle,'');

 {All pascal objects must be freed when you are finished with them, pascal doesn't
  have a garbage collector like some languages so you simply call the Free method
  on each object.}
 ConsoleWindowWriteLn(WindowHandle,'Freeing all of the objects');
 StringList1.Free;
 StringList2.Free;
 MemoryStream.Free;

 {All done, halt the thread}
 ThreadHalt(0);
end.

