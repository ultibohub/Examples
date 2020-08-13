program PWMPCM;

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}

{ Advanced example - PWM PCM                                                   }
{                                                                              }
{ This example shows how to load a PCM sound sample from a file and play it on }
{ the Pi using the PWM audio device.                                           }
{                                                                              }
{ Unlike Example 20 which uses DMA to play the sound this example feeds the    }
{ samples directly to the PWM FIFO buffer using the CPU.                       }
{                                                                              }
{ This is intended to reproduce similar examples that are available for other  }
{ such as the PIC Sound Player project at Enide!                               }
{  See: http://www.enide.net/webcms/index.php?page=pcm2pwm                     }
{                                                                              }
{ The example also shows a technique for extending the functionality of an     }
{ existing Ultibo driver by replacing some of the built in methods.            }
{                                                                              }
{ This example is specific to the Raspberry Pi because it uses the PWM device  }
{ directly instead of using a generic audio device for playback.               }
{                                                                              }
{ This version is for Raspberry Pi 3B and will also work on a 3B+/3A+.         }
{ To create a version for the A/B/A+/B+/Zero/2B simply create a new project,   }
{ copy this code into it and modify where indicated below.                     }
{                                                                              }
{  For A/B/A+/B+/Zero search for BCM2710 and change to BCM2708                 }
{                then search for BCM2837 and change to BCM2835                 }
{                                                                              }
{  For 2B search for BCM2710 and change to BCM2709                             }
{    then search for BCM2837 and change to BCM2836                             }
{                                                                              }
{  Also locate and apply several specific changes noted in the code.           }

{Declare some units used by this example.}
uses
  RaspberryPi3, {Change this to RaspberryPi or RaspberryPi2 as required for your model}
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Classes,
  SysUtils,
  PWM,          {Include the PWM unit to allow access to the functions}
  BCM2710,      {Include the BCM2710 and BCM2837 units for access to the PWM device}
  BCM2837;      {and PWM register values and constants.}
  
{We'll need a window handle and a PWM device reference.}  
var
 Handle:THandle;
 PWM0Device:PPWMDevice;
 
const
 PWMPCM_PWM_OSC_CLOCK = 19200000;   
 PWMPCM_PWM_PLLD_CLOCK = 500000000;
 
function PWMPCMClockStart(PWM:PPWMDevice;Frequency:LongWord):LongWord; 
var
 DivisorI:LongWord;
 DivisorR:LongWord;
 DivisorF:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM: PWM Clock Start');
 {$ENDIF}
 
 {Check Frequency} 
 if Frequency = 0 then Exit;

 {Check Enabled}
 if not BCM2710PWMClockEnabled(PWM) then
  begin
   {Get Divisors}
   DivisorI:=PWMPCM_PWM_PLLD_CLOCK div Frequency;
   DivisorR:=PWMPCM_PWM_PLLD_CLOCK mod Frequency;
   DivisorF:=Trunc((DivisorR * 4096) / PWMPCM_PWM_PLLD_CLOCK);
   
   if DivisorI > 4095 then DivisorI:=4095;
  
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Set Dividers}
   PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMDIV)^:=BCM2837_CM_PASSWORD or (DivisorI shl 12) or DivisorF;
   {Delay}
   MicrosecondDelay(10);
  
   {Set Source}   
   PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^:=BCM2837_CM_PASSWORD or BCM2837_CM_CTL_SRC_PLLD;
   {Delay}
   MicrosecondDelay(10);
  
   {Start Clock}   
   PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^:=BCM2837_CM_PASSWORD or PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^ or BCM2837_CM_CTL_ENAB;
   {Delay}
   MicrosecondDelay(110);
   
   {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  DivisorI=' + IntToStr(DivisorI));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  DivisorF=' + IntToStr(DivisorF));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  PWMCTL=' + IntToHex(PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^,8));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  PWMDIV=' + IntToHex(PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMDIV)^,8));
   {$ENDIF}
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end; 

function PWMPCMStart(PWM:PPWMDevice):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM: PWM Start');
 {$ENDIF}
 
 {Check Settings}
 if PWM.Range = 0 then Exit;
 if PWM.Frequency = 0 then Exit;
 
 {Check GPIO}
 if PWM.GPIO = GPIO_PIN_UNKNOWN then
  begin
   {Check Channel}
   case PBCM2710PWMDevice(PWM).Channel of
    0:begin
      {Set GPIO 18}
      if BCM2710PWMSetGPIO(PWM,GPIO_PIN_18) <> ERROR_SUCCESS then Exit;
     end; 
    1:begin
      {Set GPIO 19}
      if BCM2710PWMSetGPIO(PWM,GPIO_PIN_19) <> ERROR_SUCCESS then Exit;
     end;
    else
     begin
      Exit;
     end;   
   end;   
  end;
  
 {Start Clock}
 if PWMPCMClockStart(PWM,PWM.Frequency) <> ERROR_SUCCESS then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Enable PWEN, USEF and CLRF}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL or BCM2837_PWM_CTL_PWEN1 or BCM2837_PWM_CTL_USEF1 or BCM2837_PWM_CTL_CLRF1;
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Enable PWEN, USEF and CLRF}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL or BCM2837_PWM_CTL_PWEN2 or BCM2837_PWM_CTL_USEF2 or BCM2837_PWM_CTL_CLRF1;
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {Clear Status}
 PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).STA:=LongWord(-1);

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  CTL=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  STA=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).STA,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  DMAC=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DMAC,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  RNG1=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  DAT1=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  RNG2=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG2,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  DAT2=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT2,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

function PWMPCMSetFrequency(PWM:PPWMDevice;Frequency:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM: PWM Set Frequency (Frequency=' + IntToStr(Frequency) + ')');
 {$ENDIF}
 
 {Check Frequency}
 if Frequency = 0 then Exit;
 
 {Check Pair}
 if PBCM2710PWMDevice(PWM).Pair <> nil then
  begin
   {Check Enabled}
   if PBCM2710PWMDevice(PWM).Pair.PWM.PWMState = PWM_STATE_ENABLED then Exit;
  end;
  
 {Stop Clock}
 if BCM2710PWMClockStop(PWM) <> ERROR_SUCCESS then Exit;
 
 {Check Enabled}
 if PWM.PWMState = PWM_STATE_ENABLED then
  begin
   {Start Clock}
   if PWMPCMClockStart(PWM,Frequency) <> ERROR_SUCCESS then Exit;
  end; 
 
 {Update Scaler}
 PBCM2710PWMDevice(PWM).Scaler:=NANOSECONDS_PER_SECOND div Frequency;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM:  Scaler=' + IntToStr(PBCM2710PWMDevice(PWM).Scaler));
 {$ENDIF}
 
 {Update Properties}
 PWM.Frequency:=Frequency;
 PWM.Properties.Frequency:=Frequency;
 
 {Check Pair}
 if PBCM2710PWMDevice(PWM).Pair <> nil then
  begin
   {Update Scaler}
   PBCM2710PWMDevice(PWM).Pair.Scaler:=NANOSECONDS_PER_SECOND div Frequency;
   
   {Update Properties}
   PBCM2710PWMDevice(PWM).Pair.PWM.Frequency:=Frequency;
   PBCM2710PWMDevice(PWM).Pair.PWM.Properties.Frequency:=Frequency;
  end;
  
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

function PWMPCMPlaySample(PWM:PPWMDevice;Data:Pointer;Size,ChannelCount,BitCount:LongWord):LongWord;
var
 Buffer:PByte;
 Count:LongWord;
 Value1:LongWord;
 Value2:LongWord;
 RangeBits:LongWord;
 
 Output:PWord;
 Samples:LongWord;
 Current:LongWord;

 Status:LongWord;
 
 FullCount:LongWord;
 EmptyCount:LongWord;
 GAPCount:LongWord;
begin 
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM: PWM Play Sample');
 {$ENDIF}
 
 {Check Parameters}
 if Size = 0 then Exit;
 if (ChannelCount <> 1) and (ChannelCount <> 2) then Exit;
 if (BitCount <> 8) and (BitCount <> 16) then Exit;
 
 ConsoleWindowWriteLn(Handle,'Playing ' + IntToStr(Size) + ' bytes on ' + IntToStr(ChannelCount) + ' channel(s) at ' + IntToStr(BitCount) + ' bits per channel');
 
 {Calculate Range Bits}
 RangeBits:=0;
 Count:=2;
 while Count < 16 do
  begin
   if PWM.Range < (1 shl Count) then
    begin
     RangeBits:=Count - 1;
     Break;
    end;
   
   Inc(Count); 
  end;
 ConsoleWindowWriteLn(Handle,'Range = ' + IntToStr(PWM.Range));
 ConsoleWindowWriteLn(Handle,'Range Bits = ' + IntToStr(RangeBits));
 
 {Get Sample Count}
 Samples:=0;
 if BitCount = 8 then
  begin
   Samples:=Size; 
   
   if ChannelCount = 1 then
    begin
     Samples:=Samples * 2; 
    end;
  end
 else if BitCount = 16 then
  begin
   Samples:=Size div 2;
   
   if ChannelCount = 1 then
    begin
     Samples:=Samples * 2; 
    end;
  end;  
 if Samples = 0 then Exit;
 
 Output:=GetMem(Samples * SizeOf(Word));
 if Output = nil then Exit;
 try
  ConsoleWindowWriteLn(Handle,'Total Samples = ' + IntToStr(Samples));
  
  {Convert Sound}
  Buffer:=Data;
  Count:=0;
  Current:=0;
  while Count < Size do
   begin 
    {Get channel 1}
    Value1:=Buffer[Count];
    Inc(Count);
    if BitCount > 8 then
     begin
      {Get 16 bit sample}
      Value1:=Value1 or (Buffer[Count] shl 8); 
      Inc(Count);
      
      {Convert to unsigned}
      Value1:=(Value1 + $8000) and ($FFFF);
     end;
    
    if BitCount >= RangeBits then
    begin
     Value1:=Value1 shr (BitCount - RangeBits);
    end
    else
    begin
     Value1:=Value1 shl (RangeBits - BitCount);
    end;
    
    {Get channel 2}
    Value2:=Value1;
    if ChannelCount = 2 then
     begin
      Value2:=Buffer[Count];
      Inc(Count);
      if BitCount > 8 then
       begin
        {Get 16 bit sample}
        Value2:=Value2 or (Buffer[Count] shl 8); 
        Inc(Count);
        
        {Convert to unsigned}
        Value2:=(Value2 + $8000) and ($FFFF);
       end;
      
      if BitCount >= RangeBits then
      begin
       Value2:=Value2 shr (BitCount - RangeBits);
      end
      else
      begin
       Value2:=Value2 shl (RangeBits - BitCount);
      end;
     end;
    
    {Store Sample}
    Output[Current]:=Value1;
    Output[Current + 1]:=Value2;
    Inc(Current,2);
   end;
  
  {Play Samples}
  Count:=0;
  FullCount:=0;
  EmptyCount:=0;
  GAPCount:=0;
  while Count < Samples do
   begin
    {Check FIFO}
    Status:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).STA;
    while ((Status and BCM2837_PWM_STA_FULL1) = 0) and (Count < Samples) do
     begin
      {Check for FIFO empty}
      if (Status and BCM2837_PWM_STA_EMPT1) <> 0 then
       begin
        Inc(EmptyCount);
       end; 
      
      PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).FIF1:=Output[Count];
      Inc(Count);

      {Skip channel 2}
      Inc(Count); 
      
      Status:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).STA;
     end;
     
    {Check for FIFO full}
    if (Status and BCM2837_PWM_STA_FULL1) <> 0 then
     begin
      Inc(FullCount);
      MicrosecondDelay(10);
     end; 
    
    {Check for GAP1 error}
    if (Status and BCM2837_PWM_STA_GAPO1) <> 0 then
     begin
      PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).STA:=BCM2837_PWM_STA_GAPO1;
      Inc(GAPCount);
     end;
   end; 
 finally
  FreeMem(Output);
 end;
 
 ConsoleWindowWriteLn(Handle,'FIFO Full count = ' + IntToStr(FullCount)); 
 ConsoleWindowWriteLn(Handle,'FIFO Empty count = ' + IntToStr(EmptyCount));
 ConsoleWindowWriteLn(Handle,'GAP error count = ' + IntToStr(GAPCount));
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

function PWMPCMPlayFile(PWM:PPWMDevice;const Filename: String;ChannelCount,BitCount:LongWord):LongWord;
var
 Buffer:Pointer;
 FileStream:TFileStream;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWMPCM: PWM Play File');
 {$ENDIF}
 
 {Check Parameters}
 if Length(Filename) = 0 then Exit;
 if (ChannelCount <> 1) and (ChannelCount <> 2) then Exit;
 if (BitCount <> 8) and (BitCount <> 16) then Exit;
 
 ConsoleWindowWriteLn(Handle,'Playing ' + Filename + ' on ' + IntToStr(ChannelCount) + ' channel(s) at ' + IntToStr(BitCount) + ' bits per channel');

 {Wait for SD Card}
 while not DirectoryExists ('C:\') do
  begin
   Sleep(100);
  end; 

 {Check File}
 if not FileExists(Filename) then Exit;
  
 {Open File}
 FileStream:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyNone);
 try
  {Check Size}
  if FileStream.Size > (100 * 1024 * 1024) then Exit;
  
  Buffer:=GetMem(FileStream.Size);
  try
   FileStream.Read(Buffer^,FileStream.Size);
   
   Result:=PWMPCMPlaySample(PWM,Buffer,FileStream.Size,ChannelCount,BitCount);
   if Result <> ERROR_SUCCESS then Exit;
  finally
   FreeMem(Buffer);
  end;  
 finally
  FileStream.Free;
 end;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

const
 SOUND_BITS = 8;
 SOUND_CHANNELS = 2;
 SAMPLE_RATE = 8000;
 CLOCK_RATE = 250000000; {For Raspberry Pi or Raspberry Pi 2 change this to 125000000}
 
begin
 {Create a console window and display a welcome message}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
 ConsoleWindowWriteLn(Handle,'Welcome to PWM PCM advanced example');
 ConsoleWindowWriteLn(Handle,'Make sure you have a the Raspberry Pi audio jack connected to the AUX input of an amplifier, TV or other audio device');

 {First locate the PWM device
 
  The Raspberry Pi has two PWM channels which will normally end up with
  the names PWM0 and PWM1 when the driver is included in an application.
  
  For this example we only need PWM0, you can setup both channels to play
  stereo audio and the decoding function above includes 2 channel support
  but we'll leave that as an exercise for your to experiment with}
 PWM0Device:=PWMDeviceFindByName('PWM0');
 if PWM0Device <> nil then
  begin
   {Modify PWM device functions.
   
    This allows us to change the behaviour of the PWM driver so we can 
    use a different clock source and enable the FIFO for audio output}
   PWM0Device.DeviceStart:=PWMPCMStart;
   PWM0Device.DeviceSetFrequency:=PWMPCMSetFrequency;
   
   {Setup PWM device 0}
   {Set the GPIO}
   PWMDeviceSetGPIO(PWM0Device,GPIO_PIN_40);
   {Set the range} 
   PWMDeviceSetRange(PWM0Device,(CLOCK_RATE + (SAMPLE_RATE div 2)) div SAMPLE_RATE);
   {And the mode to PWM_MODE_BALANCED}
   PWMDeviceSetMode(PWM0Device,PWM_MODE_BALANCED);
   {Finally set the frequency}
   PWMDeviceSetFrequency(PWM0Device,CLOCK_RATE);

   {Start the PWM device}
   if PWMDeviceStart(PWM0Device) = ERROR_SUCCESS then
    begin
     {Play the Sound Sample.
     
      If you change the file to play a different sample make sure you adjust
      the sample rate, channel count and number of bits to match your sample}
     if PWMPCMPlayFile(PWM0Device,'a2002011001-e02-8kHz.pcm',SOUND_CHANNELS,SOUND_BITS) <> ERROR_SUCCESS then
      begin
       ConsoleWindowWriteLn(Handle,'Error: Failed to play sound file');
      end
     else
      begin
       ConsoleWindowWriteLn(Handle,'Finished playing sound sample');
      end;      
     
     {Stop the PWM device}
     PWMDeviceStop(PWM0Device);
    end
   else
    begin
     ConsoleWindowWriteLn(Handle,'Error: Failed to start PWM device 0');
    end;
  end
 else
  begin
   ConsoleWindowWriteLn(Handle,'Error: Failed to locate PWM device 0');
  end;  
 
 {Turn on the LED to indicate completion} 
 ActivityLEDEnable;
 ActivityLEDOn;
 
 {Halt the thread if we return}
 ThreadHalt(0);
end.
