program Sensormatic3000;

{$mode delphi}{$H+}

{ The Ultibo Sensormatic 3000                                                  }
{                                                                              }
{ A "real world" example of what can be done with Ultibo core using the units  }
{ included in the installation and standard RTL functions. This example was    }
{ created for our presentation to the Australian Delphi User Group (ADUG) 2016 }
{ Symposium held in Melbourne and Sydney.                                      }
{                                                                              }
{ The Sensormatic 3000 represents an imaginary sensor unit that collects data  }
{ every second and records it (in memory) using a SensorData class. In reality }
{ the data is psuedo random and created by a thread which wakes every second   }
{ in order to generate a new sensor reading.                                   }
{                                                                              }
{ A separate thread draws a line graph of the data on a graphics console window}
{ while a third thread updates the time and date in the console display.       }
{                                                                              }
{ The project demonstrates threads, critical sections, memory, classes, date   }
{ and time, graphics and text console windows, logging, web server and custom  }
{ web pages with dynamic content including a JavaScript graph that shows the   }
{ same information as the on screen graph.                                     }
{                                                                              }
{ The JavaScript graph is accessible from http://<IP Address>/graph            }
{ The web status pages are available from http://<IP Address>/status           }
{                                                                              }
{ To run the example the following files need to be in the C:\www folder       }
{  Chart.bundle.js                                                             }
{  jquery.min.js                                                               }
{                                                                              }
{ To compile the program select Run, Compile (or Run, Build) from the menu.    }

uses
  {Initialization unit to adjust the default behavior}
  InitUnit,
  
  {The standard Ultibo units included by default}
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  
  {Some extra Ultibo units for shell, RTC and shell commands}
  ConsoleShell,
  DS1307,
  RemoteShell,
  ShellFilesystem,
  ShellUpdate,
  
  {Plus some extra font units which register themselves during boot}
  Latin1_8x8,
  Latin1_8x14,
  Sun_12x22,
  Bitstream_Vera_Sans_Mono_25,
  Bitstream_Vera_Sans_Mono_40, {Bitstream Vera Sans Mono font thanks to The Gnome Project}
  
  {The individual units for our project}
  MainUnit,
  GraphUnit,
  SensorUnit,
  TimeUnit;

begin
  {Not much here, start the program, halt the main thread}
  SensormaticStart;
  
  {Notice that I said "Halt the main thread", there is nothing really special about the main
   thread in Ultibo core. In fact it is not even the actual thread that starts the computer}
  ThreadHalt(0);
  
end.

