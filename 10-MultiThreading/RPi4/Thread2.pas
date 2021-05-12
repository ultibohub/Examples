unit Thread2;

{$mode objfpc}{$H+}

interface

uses
  GlobalConst, GlobalTypes, Threads, Classes, SysUtils, Console;

{This is the object based thread for our example.

 We need to define a class for our thread object, it will be based on the class
 TThread.

 The only method we need to define is Execute which is where the thread will do
 its work.}
type
 TThread2Object = class(TThread)
  public
   procedure Execute; override;
 end;

implementation

procedure TThread2Object.Execute;
var
 Count:Integer;
 WindowHandle:TWindowHandle;
begin
 {This is the thread function for our TThread2Object. When Thread2 is created it
  will start running here and will continue to run until it exits here.

  Like our other thread we'll use an endless loop and create a console window
  to print some output.}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMRIGHT,False);

 {Set the thread name of this thread to Example Thread2}
 ThreadSetName(GetCurrentThreadId,'Example Thread2');

 {And write something to the console to say we're here.}
 ConsoleWindowWriteLn(WindowHandle,'Hi, this is thread ' + ThreadGetName(ThreadID) + ' my thread ID is ' + IntToHex(ThreadID,8));
 Count:=0;

 while True do
  begin
   {Update our loop counter}
   Inc(Count);

   {And print that on our console window}
   ConsoleWindowWriteLn(WindowHandle,'Thread ' + ThreadGetName(ThreadID) + ' count is now ' + IntToStr(Count));

   {Sleep so other threads can use the CPU time.}
   Sleep(Random(6000));
  end;
end;

end.

