unit Thread1;

{$mode objfpc}{$H+}

interface

uses
  GlobalConst, GlobalTypes, Threads, SysUtils, Console;

{This is a standard Free Pascal RTL thread, it needs a thread function to run
 which is defined here, the implementation of the function is below.}
function Thread1Execute(Parameter:Pointer):PtrInt;

implementation

function Thread1Execute(Parameter:Pointer):PtrInt;
var
 Count:Integer;
 WindowHandle:TWindowHandle;
begin
 Thread1Execute:=0;

 {This is the thread function for Thread1, when Thread1 is created it will start
  running here and will continue to run until it exits here or calls EndThread.

  Most threads will do something in a loop and only exit when they have either
  completed their task or been signalled by another thread to end.

  For this example we'll use an endless loop but first let's create a console
  window to print some output.}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,False);

 {We'll also set the thread name to something meaningful so we can use it in our output}
 ThreadSetName(GetCurrentThreadId,'Example Thread1');

 {And write something to the console to say we're here.}
 ConsoleWindowWriteLn(WindowHandle,'Hi, this is thread ' + ThreadGetName(GetCurrentThreadId) + ' my thread ID is ' + IntToHex(GetCurrentThreadId,8));
 Count:=0;

 while True do
  begin
   {Let's keep a count of how many times this thread loops}
   Inc(Count);

   {And then we'll print that on our console window}
   ConsoleWindowWriteLn(WindowHandle,'Thread ' + ThreadGetName(GetCurrentThreadId) + ' count is now ' + IntToStr(Count));

   {Even though Ultibo uses preemptive thread scheduling every thread should either
    wait or sleep when it has nothing to do, that way other threads can use the CPU
    time for their work.}
   Sleep(Random(4000));
  end;
end;

end.

