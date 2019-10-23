unit p600emuclasses;

{$mode delphi}

interface

uses
  Classes, SysUtils, raze, LazLogger, math, windows;

const
  cTickMilliseconds=7;
  cZ80Frequency=4000000;
  cZ80CyclesPerTick=(cZ80Frequency div 1000) * cTickMilliseconds;
  cEmulationQuantum=4;

  cP600VoiceCount = 6;
  cDisplayTimeout = 80000;

type
  TP600Pot=(ppTune=0,ppValue,ppSpeed,ppTrkVol,ppMod,ppPitch,ppDACP,ppDACM);

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
    pgChorusOn=0,pgAudioEnable,pgToTape,pgCntIntMask
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

    FCountDone: Boolean;
    FPICPhase, FPICCtr, FPICPre: Integer;
    F7MsPhase, F7MsPre: Integer;
    F7MsInt: Boolean;

    FDACValue:Word;
    FSHAD,FSHEnable:Byte;

    FIncompleteCycles:Double;

    FRom:array[0..16*1024-1] of Byte;
    FRam:array[0..10*1024-1] of Byte;
    FPotValues:array[TP600Pot] of Word;

    FDisplayTimeout:array[0..5] of Integer;
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

implementation

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
  for i:=0 to cZ80CyclesPerTick div cEmulationQuantum - 1 do
  begin
    z80_emulate(cEmulationQuantum);
    HW.RunCycles(cEmulationQuantum);
  end;
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
  Result := False;
  if (FSHAD shr 5) >= 2 then
    Result:=Integer(FPotValues[TP600Pot((FSHAD shr 5) - 2)] shr 4) > FDACValue;
end;

procedure TProphet600Hardware.UpdateCVs;
var reg:Byte;
    dv: Word;
    i: Integer;
begin
  reg:=FSHAD and $7;

  dv := 65535;
  if (FSHAD shr 5) = 0 then
    dv := 4095 - FDACValue
  else if (FSHAD shr 5) = 1 then
    dv := FDACValue;

  for i := 0 to cP600VoiceCount - 1 do
    if (FSHEnable and (1 shl i)) <> 0 then
      FCVValues[TP600CV(reg+(i shl 3))] := dv;
end;

function TProphet600Hardware.GetCVHertz(ACV: TP600CV): Double;
begin
  Result:=(27.5)*power(2.0,CVVolts[ACV]/0.4);
end;

function TProphet600Hardware.GetGateValues(AGate: TP600Gate): Boolean;
begin
  case AGate of
    pgChorusOn:
      Result:= FIOCSData[4] and $01 <> 0;
    pgAudioEnable:
      Result:= FIOCSData[4] and $02 <> 0;
    pgToTape:
      Result:= FIOCSData[4] and $04 <> 0;
    pgCntIntMask:
      Result:= FIOCSData[4] and $08 <> 0;
  end;
end;

function TProphet600Hardware.GetCVValues(ACV: TP600CV): Word;
begin
  Result:=FCVValues[ACV];
end;

function TProphet600Hardware.GetCVVolts(ACV: TP600CV): Double;
begin
  if CVValues[ACV] > 4096 then
    Result:=-1.0
  else
    Result:=(CVValues[ACV] / 4095.0) * 4.0;
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

procedure TProphet600Hardware.SetPotValues(APot: TP600Pot; AValue: Word);
begin
  FPotValues[APot]:=AValue;
end;

procedure TProphet600Hardware.Initialize;
begin
  FillChar(FRam[0], SizeOf(FRam), $ff);

  FillChar(FOldDisplay[0], SizeOf(FOldDisplay), 0);
  FillChar(FDisplay[0], SizeOf(FDisplay), 0);
  FillChar(FDisplayTimeout[0], SizeOf(FDisplayTimeout), 0);

  FillChar(FCVValues[pcOsc6], SizeOf(FCVValues), 0);
  FillChar(FKeyStates[0], SizeOf(FKeyStates), 0);
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

procedure TProphet600Hardware.Write(AIsIO: Boolean; AAddress: Word; AValue: Byte);
var
  i: Integer;
  lv,lh: Byte;
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
    AAddress := AAddress and $ff;

    //debugln(['W ',Ord(AIsIO),' ',hexStr(AAddress,4),' ',hexStr(AValue,2),' (',AValue,')']);

    if AAddress and $10 <> 0 then
    begin
      // U204
      FIOCSData[AAddress and $07]:=AValue;

      case AAddress and $07 of
        3:
        begin
          // display
          lv := FIOCSData[2];
          lh := FIOCSData[3];
          for i := 0 to High(FDisplay) do
            if (lh and (1 shl i)) <> 0 then
            begin
              //FOldDisplay[i] := FDisplay[i];
              FDisplayTimeout[i] := cDisplayTimeout;
              FDisplay[i] := lv;
            end;
        end;
      end;
    end
    else
    begin
      // DAC S&H
      case AAddress and $0f of
        0:
        begin
          FDACValue:=(FDACValue and $0fc0) or ((Integer(AValue xor $ff) shr 2) and $3f);
          UpdateCVs;
        end;
        1:
        begin
          FDACValue:=(FDACValue and $003f) or ((Integer(AValue xor $ff) and $3f) shl 6);
          UpdateCVs;
        end;
        2:
        begin
          FSHAD := AValue;
          UpdateCVs;
        end;
        3:
        begin
          FSHEnable := AValue;
          UpdateCVs;
        end;
        4..7:
        begin
          FPICPre := AValue;
        end;
        8:
        begin
          FCountDone := False;
        end;
        $e:
        begin
          F7MsPhase := 0;
          F7MsInt := False;
          F7MsPre := (F7MsPre and $ff00) or Integer(AValue);
        end;
        $f:
        begin
          F7MsPre := (F7MsPre and $00ff) or (Integer(AValue) shl 8);
        end;
      end;
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
    AAddress := AAddress and $ff;

    //debugln(['R ',Ord(AIsIO),' ',hexStr(AAddress,4),' ',hexStr(Result,2),' (',Result,')']);

    if AAddress and $10 <> 0 then
    begin
      if AAddress and $07 = $05 then // /MISC_IN
      begin
        Result:=$05;
        if ADCCompare then
          Result:=Result or $80;
        if FPICCtr <= 0 then
          Result:=Result or $02;
      end
      else if AAddress and $07 = $01 then // /SWITCH_IN
      begin
        bIdx:=(FIOCSData[0] shr 5) * 16;
        Result:=$80;
        for i:=0 to 4 do
          Result:=Result or (ifthen(FKeyStates[i+bIdx],1,0) shl i);
      end;
    end
    else
    begin
      case AAddress and $0f of
        4..7:
        begin
          FPICCtr := FPICPre;
        end;
        $c:
        begin
          Result := Ord(FCountDone);
        end;
      end;
    end;
  end;
end;

procedure TProphet600Hardware.RunCycles(ACount: Integer);
var cv,cvAmp,cvHi:TP600CV;
    cvV:Word;
    i,cycles:Integer;
    ratio:Double;
    irq: Boolean;
begin
  // display remanance handling

  for i := 0 to High(FDisplay) do
  begin
    FDisplayTimeout[i] -= ACount;
    if FDisplayTimeout[i] < 0 then
      FOldDisplay[i] := FDisplay[i];
  end;

  // 7ms interrupt timer

  F7MsPhase += ACount;
  if F7MsPhase >= F7MsPre then
  begin
    F7MsPhase -= F7MsPre;
    F7MsInt := True;
  end;

  // programmable interrupt counter

  FPICPhase += ACount;
  if FPICPhase >= 4 * 256 then
  begin
    FPICPhase -= 4 * 256;
    FPICCtr -= 1;
  end;

  // int handling

  irq := F7MsInt or ((FPICCtr <= 0) and ((FIOCSData[4] and 8) <> 0));
  if irq then
  begin
    z80_raise_IRQ($ff);
    z80_lower_IRQ;
  end;

  // timer 2 runs at audio output frequency

    // find highest pitched osc
  cvV:=0;
  cvHi:=pcShape1; // dummy
  for i:=0 to cP600VoiceCount - 1 do
  begin
    cvAmp:=TP600CV(Ord(pcAmp6)+i*abs(Ord(pcOsc5)-Ord(pcOsc6)));
    if CVValues[cvAmp]=0 then
      Continue;

    // osc
    cv:=TP600CV(Ord(pcOsc6)+i*abs(Ord(pcOsc5)-Ord(pcOsc6)));
    if CVValues[cv]>cvV then
    begin
      cvHi:=cv;
      cvV:=CVValues[cv];
    end;

    // filter self oscillation
    if CVValues[pcRes6]>60000 then
    begin
      cv:=TP600CV(Ord(pcFil6)+i*abs(Ord(pcOsc5)-Ord(pcOsc6)));
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
    ratio:=ACount / cZ80Frequency;
    FIncompleteCycles:=FIncompleteCycles + CVHertz[cvHi] * ratio;
    cycles:=Trunc(FIncompleteCycles);
    FIncompleteCycles:=FIncompleteCycles-cycles;

    Assert(cycles in [0,1]); // quantum is too big if this fails
  end;
end;

end.

