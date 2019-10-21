unit p600emuclasses;

{$mode delphi}

interface

uses
  Classes, SysUtils, raze, LazLogger, math, windows;

type
  TP600Pot=(ppTune=0,ppValue=2,ppSpeed=4,ppTrkVol=6,ppMod=8,ppPitch=10,ppDACP=12,ppDACM=14);

  TP600LED=(
    pl0=$10,pl1=$11,pl2=$12,pl3=$13,pl4=$14,pl5=$15,pl6=$16,pl7=$17,
    pl8=$18,pl9=$19,plA=$1A,plB=$1b,plC=$1c,plD=$1d,plPrmEd=$1e,plPrgRec=$1f,
    plT1=$20,plT2=$21,plT3=$22,plT4=$23,plT5=$24,plT6=$25,plStack=$26,plRecord=$27,
    plSA=$28,plSB=$29,plSC=$2a,plSD=$2b,plUD=$2c,plAssign=$2d,plAppend=$2e,plChorus=$2f
  );
  TP600LEDStates=set of TP600LED;

  TP600CV=(
    pcOsc6=$00,pcAmp6,pcRes6,pcFil6,pcMix6,pcMod6,pcPW6,pcShape6,
    pcOsc5=$08,pcAmp5,pcRes5,pcFil5,pcMix5,pcMod5,pcPW5,pcShape5,
    pcOsc4=$10,pcAmp4,pcRes4,pcFil4,pcMix4,pcMod4,pcPW4,pcShape4,
    pcOsc3=$18,pcAmp3,pcRes3,pcFil3,pcMix3,pcMod3,pcPW3,pcShape3,
    pcOsc2=$20,pcAmp2,pcRes2,pcFil2,pcMix2,pcMod2,pcPW2,pcShape2,
    pcOsc1=$28,pcAmp1,pcRes1,pcFil1,pcMix1,pcMod1,pcPW1,pcShape1
  );

  TP600Gate=(
    pgChorusOn=0,pgAudioEnable
  );

  TP600Button=(
    pb0=$00,pb1,pb2,pb3,pb4,
    pb5=$10,pb6,pb7,pb8,pb9,
    pbAD=$20,pbPrmEd,pbPrgRec,pbStack,
    pbChorus=$30,pbMidiCh,pbMidiMode,pbToTape,pbFromTape,
    pbT1=$40,pbSeq,pbUD,pbAssign,pbRecord,
    pbT2=$50,pbT3,pbT4,pbT5,pbT6,
    pbStart=$60,pbAutoCor,pbMetro,pbAppend,pbTP
  );

  { TProphet600Hardware }

  TProphet600Hardware=record
  private
    FIOCSData:array[0..7] of Byte;

    FDACValue:Word;
    FMuxedPot:TP600Pot;
    FDACDemux:Byte;

    FIncompleteCycles:Double;

    FRom:array[0..16*1024-1] of Byte;
    FRam:array[0..10*1024-1] of Byte;
    FPotValues:array[TP600Pot] of Word;
    FOldDisplay:array[0..5] of Byte;
    FDisplay:array[0..5] of Byte;
    FCVValues:array[TP600CV] of Word;
    FKeyStates:array[0..127] of Boolean;

    function ADCCompare:Boolean;
    procedure UpdateCVs;

    // getters/setters
    function GetCVValues(ACV: TP600CV): Word;
    function GetCVVolts(ACV: TP600CV): Double;
    function GetCVHertz(ACV: TP600CV): Double;
    function GetGateValues(AGate: TP600Gate): Boolean;
    function GetLEDStates: TP600LEDStates;
    function GetPotValues(APot: TP600Pot): Word;
    function GetSevenSegment(AIndex: Integer): Byte;
    procedure SetButtonStates(AButton: TP600Button; AValue: Boolean);
    procedure SetKeyStates(AKey: Integer; AValue: Boolean);
    procedure SetPotValues(APot: TP600Pot; AValue: Word);
  public
    procedure Initialize;

    procedure LoadRomFromFile(AFileName:String);
    procedure Write(AIsIO:Boolean;AAddress:Word;AValue:Byte);
    function Read(AIsIO:Boolean;AAddress:Word):Byte;

    procedure RunCycles(ACount:Integer); // run ACount 4Mhz cycles

    property PotValues[APot:TP600Pot]:Word read GetPotValues write SetPotValues;
    property SevenSegment[AIndex:Integer]:Byte read GetSevenSegment;
    property LEDStates:TP600LEDStates read GetLEDStates;

    property ButtonStates[AButton:TP600Button]:Boolean write SetButtonStates;
    property KeyStates[AKey:Integer]:Boolean write SetKeyStates;

    property CVValues[ACV:TP600CV]:Word read GetCVValues;
    property CVVolts[ACV:TP600CV]:Double read GetCVVolts;
    property CVHertz[ACV:TP600CV]:Double read GetCVHertz;
    property GateValues[AGate:TP600Gate]:Boolean read GetGateValues;
  end;


  { TProphet600Emulator }

  TProphet600Emulator=record
  private
    FHW:TProphet600Hardware;
  public
    procedure Initialize;

    procedure Tick; // advance one 5ms tick

    property HW:TProphet600Hardware read FHW;
  end;

var
  P600Emu:TProphet600Emulator;

const
  CTickMilliseconds=5;

implementation

const
  CZ80Frequency=4000000;
  CZ80CyclesPerTick=(CZ80Frequency div 1000) * CTickMilliseconds;
  CEmulationQuantum=4;

procedure P600Emu_WriteMem(AAddress:Word;AValue:Byte);cdecl;
begin
  P600Emu.HW.Write(False,AAddress,AValue);
end;

function P600Emu_ReadMem(AAddress:Word):Byte;cdecl;
begin
  Result:=P600Emu.HW.Read(False,AAddress);
end;

procedure P600Emu_WriteIO(AAddress:Word;AValue:Byte);cdecl;
begin
  P600Emu.HW.Write(True,AAddress,AValue);
end;

function P600Emu_ReadIO(AAddress:Word):Byte;cdecl;
begin
  Result:=P600Emu.HW.Read(True,AAddress);
end;

procedure P600Mockup_Write(AIsIO:Byte;AAddress:Word;AData:Byte);stdcall;
begin
  P600Emu.HW.Write(AIsIO<>0,AAddress,AData);
end;

function P600Mockup_Read(AIsIO:Byte;AAddress:Word):Byte;stdcall;
begin
  Result:=P600Emu.HW.Read(AIsIO<>0,AAddress);
end;

procedure P600Mockup_Debug(AChar:AnsiChar);stdcall;
begin
  DbgOut([AChar]);
end;

{ TProphet600Emulator }

procedure TProphet600Emulator.Initialize;
begin
  HW.Initialize;
  HW.LoadRomFromFile(ExtractFilePath(ParamStr(0))+'mtrak.bin');

  z80_init_memmap;
  z80_map_fetch($0000,$3fff,@HW.FRom[0]);
  z80_add_read($0000,$ffff,Z80_MAP_HANDLED,@P600Emu_ReadMem);
  z80_add_write($0000,$ffff,Z80_MAP_HANDLED,@P600Emu_WriteMem);
  z80_end_memmap;

  z80_set_in(@P600Emu_ReadIO);
  z80_set_out(@P600Emu_WriteIO);

  z80_reset;
end;

procedure TProphet600Emulator.Tick;
var i:Integer;
begin
  for i:=0 to CZ80CyclesPerTick div CEmulationQuantum - 1 do
  begin
    z80_emulate(CEmulationQuantum);
    HW.RunCycles(CEmulationQuantum);
  end;

  z80_raise_IRQ($ff);
  z80_lower_IRQ;
end;

{ TProphet600Hardware }

function TProphet600Hardware.GetLEDStates: TP600LEDStates;
var v2,v3,v4,v5:Byte;
begin
  Result:=[];

  v2:=FDisplay[2] or FOldDisplay[2];
  v3:=FDisplay[3] or FOldDisplay[3];
  v4:=FDisplay[4] or FOldDisplay[4];
  v5:=FDisplay[5] or FOldDisplay[5];

  if v2 and $01 <> 0 then Result:=Result + [pl0];
  if v2 and $02 <> 0 then Result:=Result + [pl1];
  if v2 and $04 <> 0 then Result:=Result + [pl2];
  if v2 and $08 <> 0 then Result:=Result + [pl3];
  if v2 and $10 <> 0 then Result:=Result + [pl4];
  if v2 and $20 <> 0 then Result:=Result + [pl5];
  if v2 and $40 <> 0 then Result:=Result + [pl6];
  if v2 and $80 <> 0 then Result:=Result + [pl7];

  if v3 and $01 <> 0 then Result:=Result + [pl8];
  if v3 and $02 <> 0 then Result:=Result + [pl9];
  if v3 and $04 <> 0 then Result:=Result + [plA];
  if v3 and $08 <> 0 then Result:=Result + [plB];
  if v3 and $10 <> 0 then Result:=Result + [plC];
  if v3 and $20 <> 0 then Result:=Result + [plD];
  if v3 and $40 <> 0 then Result:=Result + [plPrgRec];
  if v3 and $80 <> 0 then Result:=Result + [plPrmEd];

  if v4 and $01 <> 0 then Result:=Result + [plT1];
  if v4 and $02 <> 0 then Result:=Result + [plT2];
  if v4 and $04 <> 0 then Result:=Result + [plT3];
  if v4 and $08 <> 0 then Result:=Result + [plT4];
  if v4 and $10 <> 0 then Result:=Result + [plT5];
  if v4 and $20 <> 0 then Result:=Result + [plT6];
  if v4 and $40 <> 0 then Result:=Result + [plStack];
  if v4 and $80 <> 0 then Result:=Result + [plRecord];

  if v5 and $01 <> 0 then Result:=Result + [plSA];
  if v5 and $02 <> 0 then Result:=Result + [plSB];
  if v5 and $04 <> 0 then Result:=Result + [plSC];
  if v5 and $08 <> 0 then Result:=Result + [plSD];
  if v5 and $10 <> 0 then Result:=Result + [plUD];
  if v5 and $20 <> 0 then Result:=Result + [plAssign];
  if v5 and $40 <> 0 then Result:=Result + [plAppend];
  if v5 and $80 <> 0 then Result:=Result + [plChorus];
end;

function TProphet600Hardware.ADCCompare: Boolean;
begin
  Result:=FPotValues[FMuxedPot]>(65535-FDACValue);
end;

procedure TProphet600Hardware.UpdateCVs;
var reg:Byte;
begin
  reg:=FDACDemux and $7;

  if FDACDemux and $08 = 0 then
    FCVValues[TP600CV(reg+$00)]:=(65535-FDACValue);
  if FDACDemux and $10 = 0 then
    FCVValues[TP600CV(reg+$08)]:=(65535-FDACValue);
  if FDACDemux and $20 = 0 then
    FCVValues[TP600CV(reg+$10)]:=(65535-FDACValue);
  if FDACDemux and $40 = 0 then
    FCVValues[TP600CV(reg+$18)]:=(65535-FDACValue);
end;

function TProphet600Hardware.GetCVHertz(ACV: TP600CV): Double;
begin
  Result:=(27.5)*power(2.0,CVVolts[ACV]/0.5);
end;

function TProphet600Hardware.GetGateValues(AGate: TP600Gate): Boolean;
begin
  case AGate of
    pgChorusOn:
      Result:= FIOCSData[4] and $01 <> 0;
    pgAudioEnable:
      Result:= FIOCSData[4] and $02 <> 0;
  end;
end;

function TProphet600Hardware.GetCVValues(ACV: TP600CV): Word;
begin
  Result:=FCVValues[ACV];
end;

function TProphet600Hardware.GetCVVolts(ACV: TP600CV): Double;
begin
  Result:=(CVValues[ACV] / 65535.0) * 5.0;
end;

function TProphet600Hardware.GetPotValues(APot: TP600Pot): Word;
begin
  Result:=FPotValues[APot];
end;

function TProphet600Hardware.GetSevenSegment(AIndex: Integer): Byte;
begin
  Result:=(FDisplay[AIndex] or FOldDisplay[AIndex]) and $ff;
end;

procedure TProphet600Hardware.SetButtonStates(AButton: TP600Button;
  AValue: Boolean);
begin
  FKeyStates[Ord(AButton)]:=AValue;
end;

procedure TProphet600Hardware.SetKeyStates(AKey: Integer; AValue: Boolean);
begin
  FKeyStates[64+AKey]:=AValue;
end;

procedure TProphet600Hardware.SetPotValues(APot: TP600Pot; AValue: Word);
begin
  FPotValues[APot]:=AValue;
end;

procedure TProphet600Hardware.Initialize;
begin

end;

procedure TProphet600Hardware.LoadRomFromFile(AFileName: String);
var fs:TFileStream;
begin
  fs:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    Assert(fs.Size=Length(FRom));
    fs.Read(FRom,Length(FRom));
  finally
    fs.Free;
  end;
end;

procedure TProphet600Hardware.Write(AIsIO: Boolean; AAddress: Word; AValue: Byte
  );

var reg:Byte;
begin
  if not AIsIO then
  begin
    case AAddress of
      $0000..$3fff:
        Assert(False);
      $4000..$67ff:
        FRam[AAddress-$4000]:=AValue;
      //TODO:
      //$4000:
      //begin
      //  FDACValue:=(FDACValue and $fc00) or (Integer(AValue xor $ff) shl 2) or $03;
      //  UpdateCVs;
      //end;
      //$4001:
      //begin
      //  FDACValue:=(FDACValue and $03ff) or ((Integer(AValue xor $ff) and $3f) shl 10);
      //  UpdateCVs;
      //end;
    end;
  end
  else
  begin
    if AAddress and $10 <> 0 then
    begin
      // U322
      FIOCSData[AAddress and $07]:=AValue;

      case AAddress and $07 of
        $00,$01:
        begin
          // display
          if FIOCSData[0] and $10 <> 0 then
          begin
            FOldDisplay[0]:=FDisplay[0];
            FDisplay[0]:=FIOCSData[1];
          end;
          if FIOCSData[0] and $20 <> 0 then
          begin
            FOldDisplay[1]:=FDisplay[1];
            FDisplay[1]:=FIOCSData[1];
          end;
          if FIOCSData[0] and $40 <> 0 then
          begin
            FOldDisplay[2]:=FDisplay[2];
            FDisplay[2]:=FIOCSData[1];
          end;
        end;
        $02:
        begin
          // pot mux
          reg:=AValue and $0f;

          if AValue and $10 = 0 then
            FMuxedPot:=TP600Pot(reg);
          if AValue and $20 = 0 then
            FMuxedPot:=TP600Pot(reg+$10);
        end;
        $05:
        begin
          // dac
          FDACDemux:=AValue;
          UpdateCVs;
        end;
        $06:
        begin

        end;
      end;
    end

    else
    begin

    end;
  end;
end;

function TProphet600Hardware.Read(AIsIO: Boolean; AAddress: Word): Byte;
var i,bIdx:Integer;
begin
  Result:=$ff;

  if not AIsIO then
  begin
    case AAddress of
      $0000..$3fff:
        Result:=FRom[AAddress];
      $4000..$67ff:
        Result:=FRam[AAddress-$4000];
    end;
  end
  else
  begin
    if AAddress and $10 <> 0 then
    begin
      if AAddress and $07 = $05 then // CSI0 (misc driver)
      begin
        Result:=0;
        if ADCCompare then
          Result:=Result or $80;
//        debugln(['R ',Ord(AIsIO),' ',hexStr(AAddress,4),' ',hexStr(Result,2),' (',Result,')']);
      end
      else if AAddress and $07 = $02 then // CSI1 (switch matrix)
      begin
        bIdx:=(FIOCSData[0] and $0f)*8;
        Result:=0;
        for i:=0 to 7 do
          Result:=Result or (ifthen(FKeyStates[i+bIdx],1,0) shl i);
      end;
    end
    else
    begin

    end;
  end;

end;

procedure TProphet600Hardware.RunCycles(ACount: Integer);
var cv,cvAmp,cvHi:TP600CV;
    cvV:Word;
    i,cycles:Integer;
    ratio:Double;
begin
  // timer 2 runs at audio output frequency

    // find highest pitched osc
  cvV:=0;
  cvHi:=pcShape1; // dummy
  for i:=0 to 5 do
  begin
    cvAmp:=TP600CV(Ord(pcAmp6)+i);
    if CVValues[cvAmp]=0 then
      Continue;

    // osc
    cv:=TP600CV(Ord(pcOsc6)+i);
    if CVValues[cv]>cvV then
    begin
      cvHi:=cv;
      cvV:=CVValues[cv];
    end;

    // filter self oscillation
    if CVValues[pcRes6]>60000 then
    begin
      cv:=TP600CV(Ord(pcFil6)+i);
      if CVValues[cv]>cvV then
      begin
        cvHi:=cv;
        cvV:=CVValues[cv];
      end;
    end;
  end;

  if cvHi<>pcShape1 then
  begin
      // compute cycles per ACount ticks
    ratio:=ACount / CZ80Frequency;
    FIncompleteCycles:=FIncompleteCycles + CVHertz[cvHi] * ratio;
    cycles:=Trunc(FIncompleteCycles);
    FIncompleteCycles:=FIncompleteCycles-cycles;

    Assert(cycles in [0,1]); // quantum is too big if this fails
  end;
end;

end.

