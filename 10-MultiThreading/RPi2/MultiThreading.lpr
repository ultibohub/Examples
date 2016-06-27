program MultiThreading;

{$mode objfpc}{$H+}

{ Example 10 Multi Threading                                                   }
{                                                                              }
{  Ultibo always supports multi threading, it is built in to the design and    }
{  cannot be removed.                                                          }
{                                                                              }
{  You can create as many threads as you require to run your application and   }
{  Ultibo will manage them in the background. If you need to be specific about }
{  where, when and how a thread runs you can also do that but most times you   }
{  will probably just want your threads to behave as normal.                   }
{                                                                              }
{  Ultibo uses preemptive thread scheduling so threads can be interrupted at   }
{  any time. The scheduler manages which thread gets to run next and for how   }
{  long. You can set priority levels on threads so they run more often but,    }
{  like any OS, you should be mindful to yield CPU time so that every thread   }
{  gets a fair go.                                                             }
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
  BCM2709,
  SysUtils,
  GlobalConfig, {Include the global configuration unit for thread default settings}
  Thread1,      {A couple of extra units which are part of this example, not part of Ultibo}
  Thread2;

{A window handle as usual.}
var
 WindowHandle:TWindowHandle;
 Thread1Handle:TThreadHandle;
 Thread2Object:TThread2Object;

begin
 {Create our window on the left side so there is room for more}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);

 {Output the message}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 10 Multi threading');
 ConsoleWindowWriteLn(WindowHandle,'');

 {There are basically two ways to create a thread in Ultibo, you can call the thread
  functions defined in the Free Pascal RTL or you can create a TThread object.

  In this example we'll create one thread using each of these methods so you can
  compare. There is no right or wrong way, it depends on your code which method
  you decide to use.}

 {First let's try the RTL method which uses BeginThread. There are a number of
  variations of BeginThread declared in the RTL, they all do the same job but
  some allow you to pass extra parameters to control the setup of the thread.}

 {Every thread needs at least a function to run, the thread function.}
 ConsoleWindowWriteLn(WindowHandle,'Creating Thread1 using BeginThread');
 Thread1Handle:=BeginThread(@Thread1Execute,nil,Thread1Handle,THREAD_STACK_DEFAULT_SIZE);
 if Thread1Handle = INVALID_HANDLE_VALUE then
  begin
   {If the thread handle is not valid then BeginThread failed}
   ConsoleWindowWriteLn(WindowHandle,'Failed to create Thread1');
  end
 else
  begin
   {Otherwise the thread was created and will start running soon, we have a handle
    to reference it if we want. The Thread1Execute function is in the Thread1 unit,
    have a look there to see what it will be doing.}
   ConsoleWindowWriteLn(WindowHandle,'Thread1 was created successfully, the handle is ' + IntToHex(Thread1Handle,8));

   {Let's wait a bit to see what happens}
   Sleep(5000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'');

 {Now let's create a thread using the TThread class to see how that differs.

  We need to create a class derived from TThread in order to define what work our
  new thread should do. The class for our thread is in the Thread2 unit.}
 ConsoleWindowWriteLn(WindowHandle,'Creating Thread2 using TThread2Object');
 Thread2Object:=TThread2Object.Create(False,THREAD_STACK_DEFAULT_SIZE);

 {We assume our thread was created successfully, we can check from the object what
  the thread handle is and use it in other functions.}
 ConsoleWindowWriteLn(WindowHandle,'Thread2 was created successfully, the handle is ' + IntToHex(Thread2Object.ThreadID,8));
 ConsoleWindowWriteLn(WindowHandle,'');

 {Say goodbye from this thread}
 ConsoleWindowWriteLn(WindowHandle,'Goodbye from ' + ThreadGetName(GetCurrentThreadId) + ', my ID is ' + IntToHex(GetCurrentThreadId,8));

 {We haven't talked about locks and synchronization in this example. Ultibo supports
  a full range of locking and synchronization primitives which are very important as
  soon as you start to use multiple threads. See the wiki and other resources for
  more information on these and other topics to do with multi thread programming.}

 {Halt thread, the others will keep running}
 ThreadHalt(0);
end.

