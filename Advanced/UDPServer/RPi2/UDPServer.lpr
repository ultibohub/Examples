program UDPServer;

{$mode objfpc}{$H+}

{ Advanced example - UDP Server                                                }
{                                                                              }
{ This example looks at using the TWinsock2UDPListener class in the Winsock2   }
{ unit to create a multithreaded UDP server. For the purpose of this example   }
{ we create a very basic SysLog server and set our own logging to be sent to   }
{ it.                                                                          }
{                                                                              }
{ Most of the functionality for this example is contained in the ServerUnit.pas}
{ file rather than the program file itself. Open ServerUnit to follow what is  }
{ happening with our UDP server.                                               }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{ To create a version for the A/B/A+/B+/Zero simply create a new project and   }
{ copy this code into it.                                                      }

uses
  InitUnit,     {Include InitUnit to allow us to change the startup behaviour}
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  Threads,
  ServerUnit;   {Include ServerUnit which contains the main code for our server}
 
begin
 {Like most typical Free Pascal applications, the program file itself doesn't
  contain very much. All we do here is some initialization and call the start
  function in the ServerUnit. Switch over to there to see what is happening}
  
 {Initialize our UDP Server} 
 ServerInit;
 
 {Start our UDP Server}
 ServerStart; 
 
 {Halt the thread if we return}
 ThreadHalt(0);
end.
 