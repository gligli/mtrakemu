unit p600emuclasses;

{$mode delphi}

interface

uses
  Classes, SysUtils, raze, LazLogger, math, windows;

const
  cTickMilliseconds = 7;
  cZ80Frequency = 4000000;
  cZ80CyclesPerTick = (cZ80Frequency div 1000) * cTickMilliseconds;
  cEmulationQuantum = 4;

  cP600VoiceCount = 6;
  cDisplayTimeout = 100000;

type
  TP600Pot=(
    ppDACM,ppDACP,ppPitch,ppMod,ppTrkVol,ppSpeed,ppValue,ppTune
  );

  TP600LED=(
    plA1,plB1,plC1,plD1,plE1,plF1,plG1,plDP1,
    plA2,plB2,plC2,plD2,plE2,plF2,plG2,plDP2,
    pl0,pl1,pl2,pl3,pl4,pl5,pl6,pl7,pl8,pl9,plA,plB,plC,plD,plPrmEd,plPrgRec,
    plT1,plT2,plT3,plT4,plT5,plT6,plStack,plRecord,plSA,plSB,plSC,plSD,plUD,plAssign,plAppend,plChorus
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
    pgChorusOn=0,pgAudioEnable,pgFromTape,pgCntIntMask
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
    FCurTick: Integer;

    FIOCSData:array[0..7] of Byte;

    FTuneCntLo, FTuneCntHi: Integer;
    FTuneCntInh, FTuneCntDone: Boolean;

    FPICPhase: Integer;
    FPICCtr, FPICPre: Byte;
    FPICCntComp: Boolean;
    F7MsPhase, F7MsPre: Integer;
    F7MsInt: Boolean;

    FDACValue:Word;
    FSHAD,FSHEnable:Byte;

    FIncompleteCycles:Double;

    FRom:array[0..16*1024-1] of Byte;
    FRam:array[0..10*1024-1] of Byte;
    FPotValues:array[TP600Pot] of Word;
    FDisplay:array[TP600LED] of Integer;
    FCVValues:array[TP600CV] of Integer;
    FKeyStates:array[0..127] of Boolean;

    function ADCCompare:Boolean;
    procedure UpdateCVs;

    // getters/setters
    function GetCVValues(ACV: TP600CV): Integer;
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

    property CVValues[ACV:TP600CV]:Integer read GetCVValues;
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

const
  CABCToPot : array[0..7] of TP600Pot = (ppDACP, ppTrkVol, ppMod, ppSpeed, ppDACM, ppValue, ppPitch, ppTune);

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
var
  l: TP600LED;
begin
  Result:=[];
  for l := Low(TP600LED) to High(TP600LED) do
    if FDisplay[l] > 0 then
      Result:=Result + [l];
end;

function TProphet600Hardware.ADCCompare: Boolean;
begin
  Result:=Integer(FPotValues[CABCToPot[FSHAD shr 5]] shr 4) > FDACValue;
end;

procedure TProphet600Hardware.UpdateCVs;
var reg:Byte;
    dv: Integer;
    i: Integer;
begin
  reg:=FSHAD and $7;

  dv := 65536;
  if CABCToPot[FSHAD shr 5] = ppDACM then
    dv := -FDACValue
  else if CABCToPot[FSHAD shr 5] = ppDACP then
    dv := FDACValue;

  for i := 0 to cP600VoiceCount - 1 do
    if (FSHEnable and (1 shl i)) = 0 then
      FCVValues[TP600CV(reg + (i shl 3))] := dv;
end;

function TProphet600Hardware.GetCVHertz(ACV: TP600CV): Double;
begin
  Result := ifthen(TP600CV(Ord(ACV) and $07) = pcFil6, 1300.0 * power(2.0, CVVolts[ACV] / 0.38), 500.0 * power(2.0, CVVolts[ACV] / 0.75));
end;

function TProphet600Hardware.GetGateValues(AGate: TP600Gate): Boolean;
begin
  case AGate of
    pgChorusOn:
      Result := FIOCSData[4] and $01 = 0;
    pgAudioEnable:
      Result := FIOCSData[4] and $02 = 0;
    pgFromTape:
      Result := FIOCSData[4] and $04 = 0;
    pgCntIntMask:
      Result := FIOCSData[4] and $08 = 0;
  end;
end;

function TProphet600Hardware.GetCVValues(ACV: TP600CV): Integer;
begin
  Result:=FCVValues[ACV];
end;

function TProphet600Hardware.GetCVVolts(ACV: TP600CV): Double;
begin
  if abs(CVValues[ACV]) > 4096 then
    Result:=-10.0
  else
    Result:=(CVValues[ACV] / 4095.0) * 4.0;
end;

function TProphet600Hardware.GetPotValues(APot: TP600Pot): Word;
begin
  Result:=FPotValues[APot];
end;

function TProphet600Hardware.GetSevenSegment(AIndex: Integer): Byte;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to 7 do
    Result := Result or (Ord(FDisplay[TP600LED(i + AIndex * 8)] > 0) shl i);
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
  FCurTick := 0;

  FillChar(FRam[0], SizeOf(FRam), $ff);
  FillChar(FDisplay[plA1], SizeOf(FDisplay), 0);
  FillChar(FCVValues[pcOsc6], SizeOf(FCVValues), 0);
  FillChar(FKeyStates[0], SizeOf(FKeyStates), 0);

  ButtonStates[pbTP] := True;

  FTuneCntDone := False;
  FTuneCntInh := True;
  FTuneCntHi := 0;
  FTuneCntLo := 1;

  FPICCntComp := False;
  FPICCtr := 0;
  FPICPhase := 0;
  FPICPre := 0;

  F7MsInt := False;
  F7MsPhase := 0;
  F7MsPre := 0;
  FCurTick := 0;
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
  i, j: Integer;
  lv,lh: Byte;
begin
  if not AIsIO then
  begin
    AAddress := AAddress and $7fff;

    case AAddress of
      $0000..$3fff:
        Assert(False);
      $4000..$67ff:
        FRam[AAddress and $3fff] := AValue;
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
          for i := 0 to 5 do
            if (lh and (1 shl i)) <> 0 then
              for j := 0 to 7 do
                if lv and (1 shl j) <> 0 then
                  FDisplay[TP600LED(j + i * 8)] += cDisplayTimeout
                else
                  FDisplay[TP600LED(j + i * 8)] := 0;
        end;
        4:
        begin
          // misc out
          FPICCntComp := FPICCntComp or ((AValue and 8) = 0);
        end;
      end;
    end
    else
    begin
      // DAC S&H
      case AAddress and $0f of
        0:
        begin
          FDACValue:=(FDACValue and $0fc0) or ((Integer(AValue) shr 2) and $3f);
          UpdateCVs;
        end;
        1:
        begin
          FDACValue:=(FDACValue and $003f) or ((Integer(AValue) and $3f) shl 6);
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
          FPICCtr := FPICPre;
        end;
        8:
        begin
          FTuneCntLo := Integer(AValue and $1f or $01);
          FTuneCntHi := 0;
          FTuneCntDone := False;
        end;
        $c:
        begin
          FTuneCntInh := (AValue and 8) <> 0;
        end;
        $e:
        begin
          F7MsInt := False;
          F7MsPre := (F7MsPre and $ff00) or Integer(AValue);
          F7MsPhase := F7MsPre;
        end;
        $f:
        begin
          F7MsInt := False;
          F7MsPre := (F7MsPre and $00ff) or (Integer(AValue) shl 8);
          F7MsPhase := F7MsPre;
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
    AAddress := AAddress and $7fff;

    case AAddress and $7fff of
      $0000..$3fff:
        Result := FRom[AAddress];
      $4000..$67ff:
        Result := FRam[AAddress and $3fff];
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
        Result := $05;
        if ADCCompare then
          Result := Result or $80;
        if FPICCntComp then
          Result := Result or $02;
      end
      else if AAddress and $07 = $01 then // /SWITCH_IN
      begin
        bIdx := (FIOCSData[0] shr 5) * 16;
        Result := $e0;
        for i := 0 to 4 do
          Result := Result or (ifthen(FKeyStates[i + bIdx], 1, 0) shl i);
      end;
    end
    else
    begin
      case AAddress and $0f of
        4..7:
        begin
          FPICCntComp := False;
        end;
        $a:
        begin
          Result := (FTuneCntHi shr 1) and $ff;
        end;
        $b:
        begin
          Result := (FTuneCntHi shr 9) and $ff;
        end;
        $c:
        begin
          Result := Ord(FTuneCntDone);
        end;
      end;
    end;
  end;
end;

procedure TProphet600Hardware.RunCycles(ACount: Integer);
var cv,cvHi,cvFil:TP600CV;
    cvV:Integer;
    i, cycles:Integer;
    ratio:Double;
    l: TP600LED;
begin
  FCurTick += ACount;

  // display remanance handling

  for l := Low(l) to High(l) do
  begin
    if (FIOCSData[3] and (1 shl (Ord(l) shr 3))) <> 0 then
      Continue;

    FDisplay[l] -= ACount;
    if FDisplay[l] < 0 then
      FDisplay[l] := 0;
  end;

  // tune counter

  if not FTuneCntDone then
  begin
      // find highest pitched osc
    cvV := -65536;
    cvHi := pcShape1; // dummy
    for i := 0 to cP600VoiceCount - 1 do
    begin
      cv := TP600CV(Ord(pcAmp6) + i * abs(Ord(pcOsc5) - Ord(pcOsc6)));
      if CVValues[cv] > -8 * 4096 div 16 then
        Continue;

      // osc
      cvFil := TP600CV(Ord(pcFil6) + i * abs(Ord(pcOsc5) - Ord(pcOsc6)));
      cv := TP600CV(Ord(pcOsc6) + i * abs(Ord(pcOsc5) - Ord(pcOsc6)));
      if (CVValues[cv] > cvV) and (CVHertz[cvFil] >= CVHertz[cv]) then
      begin
        cvHi := cv;
        cvV := CVValues[cv];
      end;

      // filter self oscillation
      cv := TP600CV(Ord(pcRes6) + i * abs(Ord(pcOsc5) - Ord(pcOsc6)));
      if CVValues[cv] < -8 * 4096 div 16 then
      begin
        cv := TP600CV(Ord(pcFil6) + i * abs(Ord(pcOsc5) - Ord(pcOsc6)));
        if CVValues[cv] > cvV then
        begin
          cvHi := cv;
          cvV := CVValues[cv];
        end;
      end;
    end;

    if cvHi <> pcShape1 then
    begin
        // compute cycles per ACount ticks
      ratio := ACount / cZ80Frequency * 2.0;
      FIncompleteCycles += CVHertz[cvHi] * ratio;
      cycles := Trunc(FIncompleteCycles);
      FIncompleteCycles -= cycles;

      if cycles > 0 then
      begin
        Dec(FTuneCntLo, cycles);
      end;
    end;

    // SCI combo chip part

    if FTuneCntLo < 0 then
      FTuneCntDone := True
    else if not FTuneCntInh then
      FTuneCntHi += ACount;

    //if ((FCurTick mod 100) = 0) and not FTuneCntInh then
    //  WriteLn(FTuneCntLo,#9,FTuneCntHi,#9'done',FTuneCntDone,#9'inh',FTuneCntInh);
  end;

  // 7ms interrupt timer

  F7MsPhase += ACount;
  if F7MsPhase >= 65536 then
  begin
    F7MsPhase := Low(Integer);
    F7MsInt := True;
  end;

  // programmable interrupt counter

  FPICPhase += ACount;
  if FPICPhase >= 4 * 256 then
  begin
    FPICPhase -= 4 * 256;
    FPICCtr := (FPICCtr - 1) and $ff;
  end;

  if (FPICCtr = 255) and ((FIOCSData[4] and 8) <> 0) then
    FPICCntComp := True;

  // int handling

  if F7MsInt or FPICCntComp then
    z80_raise_IRQ($38)
  else
    z80_lower_IRQ;

end;

end.

