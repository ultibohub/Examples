program MultiCPU;

{$mode objfpc}{$H+}

{ Example 11 Multi CPU                                                         }
{                                                                              }
{  Multi CPU is a fundamental part of the design of Ultibo. It does not need   }
{  to be enabled and while you can change some configuration it is generally   }
{  just there whenever the target board has more than one CPU.                 }
{                                                                              }
{  In this example we'll look at some of the multi CPU information available   }
{  in Ultibo.                                                                  }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 3B version                                                     }
{   There is, of course, no Raspberry Pi A/B/A+/B+ or Zero version.            }

{Declare some units used by this example.}
uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  BCM2837,
  BCM2710,
  SysUtils;

{A window handle as usual.}
var
 Count:Integer;
 Loops:Integer;
 WindowHandle:TWindowHandle;

begin
 {Create our window}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Output the message}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 11 Multi CPU');
 ConsoleWindowWriteLn(WindowHandle,'');

 {First print some basic information}
 ConsoleWindowWriteLn(WindowHandle,'CPU Count is ' + IntToStr(CPUGetCount));
 ConsoleWindowWriteLn(WindowHandle,'Boot CPU is ' + IntToStr(CPUGetBoot));
 ConsoleWindowWriteLn(WindowHandle,'Current CPU is ' + IntToStr(CPUGetCurrent));
 ConsoleWindowWriteLn(WindowHandle,'CPU Description is ' + CPUGetDescription);
 ConsoleWindowWriteLn(WindowHandle,'Total thread count is ' + IntToStr(ThreadGetCount));
 ConsoleWindowWriteLn(WindowHandle,'');

 {Let's do an endless loop and print some information each time}
 Loops:=0;
 while True do
  begin
   {To get the utilization for each CPU we need to call CPUGetPercentage(), but
    we need to call it once for each CPU in the system. So we'll use a loop
    to count from 0 to the number of CPUs minus 1.}
   ConsoleWindowWriteEx(WindowHandle,'CPU Utilization:',1,9,ConsoleWindowGetForecolor(WindowHandle),ConsoleWindowGetBackcolor(WindowHandle));
   for Count:=0 to CPUGetCount - 1 do
    begin
     {Check if this CPU is the one we are running on}
     if Count <> CPUGetCurrent then
      begin
       {Get the utilization and write it to the screen using ConsoleWindowWriteEx
        so we can control where it goes.}
       ConsoleWindowWriteEx(WindowHandle,' CPU' + IntToStr(Count) + ' ' + FloatToStr(CPUGetPercentage(Count)) + '%   ',1,10 + Count,ConsoleWindowGetForecolor(WindowHandle),ConsoleWindowGetBackcolor(WindowHandle));
      end
     else
      begin
       {Get the utilization and write it to the screen but set the color to raspberry
        so we can see which CPU is ours.}
       ConsoleWindowWriteEx(WindowHandle,' CPU' + IntToStr(Count) + ' ' + FloatToStr(CPUGetPercentage(Count)) + '%   ',1,10 + Count,COLOR_RASPBERRY,ConsoleWindowGetBackcolor(WindowHandle));
      end;
    end;

   {We can also get the ready thread count for each CPU to see what is queued for
    the scheduler to run. Again we loop through each CPU and write it to the screen.}
   ConsoleWindowWriteEx(WindowHandle,'Ready Thread count:',1,14,ConsoleWindowGetForecolor(WindowHandle),ConsoleWindowGetBackcolor(WindowHandle));
   for Count:=0 to CPUGetCount - 1 do
    begin
     {The SchedulerGetThreadCount function returns the number of ready threads on each
      CPU. Ready threads are those that could run immediately and doesn't include threads
      that are waiting, sleeping or already running.}
     ConsoleWindowWriteEx(WindowHandle,' CPU' + IntToStr(Count) + ' ' + IntToStr(SchedulerGetThreadCount(Count)) + '   ',1,15 + Count,ConsoleWindowGetForecolor(WindowHandle),ConsoleWindowGetBackcolor(WindowHandle));
    end;

   {Let's do the scheduler thread quantum as well which determines how long each
    thread gets to run before another thread is scheduled.}
   ConsoleWindowWriteEx(WindowHandle,'Scheduler Thread quantum:',1,19,ConsoleWindowGetForecolor(WindowHandle),ConsoleWindowGetBackcolor(WindowHandle));
   for Count:=0 to CPUGetCount - 1 do
    begin
     {The SchedulerGetThreadQuantum function returns the number of scheduler ticks
      remaining before the currently running thread will be rescheduled. A thread gives
      up its quantum whenever it sleeps or waits and the scheduler may decide to run
      a higher priority thread instead of the current thread on the next tick.}
     ConsoleWindowWriteEx(WindowHandle,' CPU' + IntToStr(Count) + ' ' + IntToStr(SchedulerGetThreadQuantum(Count)) + '   ',1,20 + Count,ConsoleWindowGetForecolor(WindowHandle),ConsoleWindowGetBackcolor(WindowHandle));
    end;

   {Now finally let's keep track of how many loops we have done and change CPUs when
    we reach a certain number.}
   Inc(Loops);

   {Check for about 100 loops, that should be about 10 seconds}
   if Loops >= 100 then
    begin
     {Call the ThreadMigrate function and specify a random CPU}
     ThreadMigrate(GetCurrentThreadId,Random(CPUGetCount - 1));

     {Reset our loop counter}
     Loops:=0;
    end;

   {Sleep a while so we don't mess with the utilization too much}
   Sleep(100);
  end;

 {No need to Halt, we never get to here.}
end.

