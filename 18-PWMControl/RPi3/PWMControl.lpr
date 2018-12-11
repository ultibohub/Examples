program PWMControl;

{$mode objfpc}{$H+}

{ Example 18 PWM Control                                                       }
{                                                                              }
{ This example demonstrates using the PWM devices in Ultibo to control two     }
{ LEDs. The PWM outputs are alternated so that the LEDs start off and gradually}
{ increase in brightness until they are fully on, the cycle then reverses so   }
{ that the LEDs gradually decrease in brightness until they are off.           }
{                                                                              }
{ To make it even more interesting the LEDs are opposite to each other so when }
{ one is increasing in brightness the other is decreasing.                     }
{                                                                              }
{ To try this example you'll need the following:                               }
{                                                                              }
{ An LED and a 330 ohm resistor connected between GPIO Pin 18 and Ground       }
{  (Ensure you connect the LED and resistor in series with each other and that }
{   the short leg or the flat side of the LED is connected to Ground)          }
{                                                                              }
{ An LED and a 330 ohm resistor connected between GPIO Pin 19 and Ground       }
{  (Ensure you connect the LED and resistor in series with each other and that }
{   the short leg or the flat side of the LED is connected to Ground)          }
{                                                                              }
{ It's usually best to use a breadboard or breakout board to connect components}
{ to the Pi just to avoid any accidental damage.                               }
{                                                                              }
{ The documentation below shows you where to find each of the pins.            }
{                                                                              }
{ Raspberry Pi Model A and B (26 pin header)                                   }
{   https://www.raspberrypi.org/documentation/usage/gpio/                      }
{                                                                              }
{ Raspberry Pi Models A+/B+/Zero/2B/3B/3B+/3A+ (40 pin header)                 }
{   https://www.raspberrypi.org/documentation/usage/gpio-plus-and-raspi2/      }
{                                                                              }
{ We've also included a diagram that shows the connections, look at the file   }
{ PWM Example.png in the same folder as this example.                          }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 3B/3B+/3A+ version                                             }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Classes,
  SysUtils,
  PWM;   {Include the PWM unit to allow access to the functions}

{Declare a window handle, a counter and a couple of PWM devices}  
var
 Handle:THandle;
 Count:Integer;
 PWM0Device:PPWMDevice;
 PWM1Device:PPWMDevice;
 
begin
 {Create a console window and display a welcome message}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
 ConsoleWindowWriteLn(Handle,'Welcome to Example 18 PWM Control');

 {First locate the PWM devices
 
  The Raspberry Pi has two PWM channels which will normally end up with the names
  PWM0 and PWM1 when the driver is included in an application.
  
  You could also use PWMDeviceFindByDescription() here and use the contants defined
  in the BCM2709 unit for BCM2709_PWM0_DESCRIPTION and BCM2709_PWM1_DESCRIPTION.
  
  Those values are for Raspberry Pi 2 so you would adjust to use BCM2708 or BCM2710 for
  the Raspberry Pi A/B/A+/B+/Zero or Raspberry Pi 3 respectively. The names of the 
  constants also change to BCM2708 or BCM2710 for the different models as well}
 
 PWM0Device:=PWMDeviceFindByName('PWM0');
 PWM1Device:=PWMDeviceFindByName('PWM1');
 if (PWM0Device <> nil) and (PWM1Device <> nil) then
  begin
   {This example uses the default GPIO pin values which are GPIO_PIN_18 for PWM0
    and GPIO_PIN_19 for PWM1. If you need to use one of the alternate GPIO pins
    then you can call PWMDeviceSetGPIO() with the required pin number. 
    
    You can also use PWMDeviceGetGPIO() to find out the currently configured pin}
   
   {On the Raspberry Pi the PWM setup requires 3 values. 
   
    The first is the Mode which can be PWM_MODE_MARKSPACE, PWM_MODE_BALANCED or 
    PWM_MODE_SERIALIZED. These are described in detail in the BCM2835 ARM Peripherals
    documentation which can be found via the resources page on the Ultibo wiki.
    
    The second value is the Frequency which controls the frequency of the clock
    used by the PWM device. On the Raspberry Pi both PWM devices share a common
    clock so changing the frequency on one device also changes it on the other.
    
    The final setup value is the Range, the exact meaning of the range value varies
    depending on the mode selected but in general it represents the time period of
    one full cycle of the waveform output by the device.

    The range and the data define what is actually output onto the GPIO pin, as an
    alternative to setting them individually you can call PWMDeviceConfigure() which
    allows you to specify both a Range and a Duty cycle in nanoseconds.
    
    Try experimenting with the range and data values to see how they affect the LEDs}
    
   {Setup PWM device 0}
   {Set the range to 1000} 
   PWMDeviceSetRange(PWM0Device,1000);
   {And the mode to PWM_MODE_MARKSPACE}
   PWMDeviceSetMode(PWM0Device,PWM_MODE_MARKSPACE);
   {Finally set the frequency to 9.6MHz}
   PWMDeviceSetFrequency(PWM0Device,9600000);
   
   {Setup PWM device 1}
   {Use exactly the same settings as PWM0}
   PWMDeviceSetRange(PWM1Device,1000);
   PWMDeviceSetMode(PWM1Device,PWM_MODE_MARKSPACE);
   PWMDeviceSetFrequency(PWM1Device,9600000);
 
   {Start the PWM devices
   
    This will start the clock and enable the devices, the final step to
    output something is to write some actual data which will specify how
    many pulses are output within the time period defined by the range.
    
    A data value of 0 will turn off the output whereas a data value equal 
    to the range will mean the output is always on (pulses are continuous).
    We can use this to make our LED go from fully off to fully on in gradual
    steps, the time it takes to make this transition is simply controlled by
    the value passed to Sleep()}
   if (PWMDeviceStart(PWM0Device) = ERROR_SUCCESS) and (PWMDeviceStart(PWM1Device) = ERROR_SUCCESS) then
    begin
     {Start an endless loop writing data values to the PWM devices}
     while True do
      begin
       {Cycle the devices through the entire range from 0 to 1000.
       
        The PWM0 device goes upwards (from off to full brightness)
        and the PWM1 device goes down (from full brightness to off)}
       for Count:=0 to 1000 do
        begin
         PWMDeviceWrite(PWM0Device,Count);
         PWMDeviceWrite(PWM1Device,1000 - Count);
         
         Sleep(1);
        end;
      
       {Reverse the directions from above so PWM0 starts at full and
        PWM1 starts at off, then repeat from the beginning}
       for Count:=0 to 1000 do
        begin
         PWMDeviceWrite(PWM0Device,1000 - Count);
         PWMDeviceWrite(PWM1Device,Count);
         
         Sleep(1);
        end;
      end;
   
     {Stop the PWM devices
     
      This will disable the devices and stop the clock, remember that the
      clock is shared between both devices so the driver will only actually
      stop the clock when PWMDeviceStop() is called for both of them}
     PWMDeviceStop(PWM0Device);
     PWMDeviceStop(PWM1Device);
    end
   else
    begin
     ConsoleWindowWriteLn(Handle,'Error: Failed to start PWM devices 0 and 1');
    end;
  end
 else
  begin
   ConsoleWindowWriteLn(Handle,'Error: Failed to locate PWM devices 0 and 1');
  end;  
  
 {Halt the thread if we return}
 ThreadHalt(0);
end.
 