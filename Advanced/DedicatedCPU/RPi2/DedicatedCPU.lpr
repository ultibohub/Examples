program DedicatedCPU;

{$mode objfpc}{$H+}

{ Advanced example - Dedicated CPU                                             }
{                                                                              }
{ This example shows how to take control of a CPU in Ultibo core and use it as }
{ a high speed micro controller doing real time operations.                    }
{                                                                              }
{ Using this technique you can take advantage of the functionality of Ultibo   }
{ core to handle things like networking, devices and files but still get real  }
{ time performance using one CPU dedicated to the task.                        }
{                                                                              }
{ !! WARNING, WARNING, WARNING, WARNING, WARNING, WARNING, WARNING, WARNING !! }
{                                                                              }
{ Using a single CPU for real time tasks in a sophisticated preemptive multi   }
{ CPU environment is NOT a free lunch, you MUST obey very strict rules about   }
{ the functions that can be called from the dedicated CPU or you will quickly  }
{ find that the result is a complete and total deadlock.                       }
{                                                                              }
{ Please read all of the comments in the StartDedicatedThread() function and   }
{ the DedicatedThreadExecute() function before you try using this in your own  }
{ projects.                                                                    }
{                                                                              }
{                     !!! You have been warned !!!                             }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{ To create a version for the A/B/A+/B+/Zero simply create a new project and   }
{ copy this code into it.                                                      }

uses
  InitUnit,     {Include InitUnit to allow us to change the startup behaviour}
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  ThreadUnit;   {Include our thread unit which contains most of the example}

  
var
 LeftWindow:TWindowHandle;
 HTTPListener:THTTPListener;
 
 
begin
 {Create a console window to show what is happening}
 LeftWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 
 {Display a startup message on the console}
 ConsoleWindowWriteLn(LeftWindow,'Starting dedicated CPU example');
 
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);
 
 {Start our dedicated CPU thread}
 StartDedicatedThread(LeftWindow);
 
 {Halt this thread}
 ThreadHalt(0);
end.
 