Case s.AddSenseCode of
  $00:
  begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_00_00;
      $06: ErrStr := ERR_AS_00_06;
      $11: ErrStr := ERR_AS_00_11;
      $12: ErrStr := ERR_AS_00_12;
      $13: ErrStr := ERR_AS_00_13;
      $14: ErrStr := ERR_AS_00_14;
      $15: ErrStr := ERR_AS_00_15;
      $16: ErrStr := ERR_AS_00_16;
      $17: ErrStr := ERR_AS_00_17;
    end;
  end;
  $02: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_02_00;
    end;
  end;
  $04: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_04_00;
      $01: ErrStr := ERR_AS_04_01;
      $02: ErrStr := ERR_AS_04_02;
      $03: ErrStr := ERR_AS_04_03;
      $04: ErrStr := ERR_AS_04_04;
      $07: ErrStr := ERR_AS_04_07;
      $08: ErrStr := ERR_AS_04_08;
      $09: ErrStr := ERR_AS_04_09;
    end;
  end;
  $05: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_05_00;
    end;
  end;
  $06: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_06_00;
    end;
  end;
  $07: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_07_00;
    end;
  end;
  $08: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_08_00;
      $01: ErrStr := ERR_AS_08_01;
      $02: ErrStr := ERR_AS_08_02;
      $03: ErrStr := ERR_AS_08_03;
      $04: ErrStr := ERR_AS_08_04;
    end;
  end;
  $09: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_09_00;
      $01: ErrStr := ERR_AS_09_01;
      $02: ErrStr := ERR_AS_09_02;
      $03: ErrStr := ERR_AS_09_03;
      $04: ErrStr := ERR_AS_09_04;
    end;
  end;
  $0A: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_0A_00;
    end;
  end;
  $0B: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_0B_00;
      $01: ErrStr := ERR_AS_0B_01;
      $02: ErrStr := ERR_AS_0B_02;
    end;
  end;
  $0C: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_0C_00;
      $07: ErrStr := ERR_AS_0C_07;
      $08: ErrStr := ERR_AS_0C_08;
      $09: ErrStr := ERR_AS_0C_09;
      $0A: ErrStr := ERR_AS_0C_0A;
    end;
  end;
  $0D: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_0D_00;
      $01: ErrStr := ERR_AS_0D_01;
      $02: ErrStr := ERR_AS_0D_02;
      $03: ErrStr := ERR_AS_0D_03;
      $04: ErrStr := ERR_AS_0D_04;
      $05: ErrStr := ERR_AS_0D_05;
    end;
  end;
  $11: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_11_00;
      $01: ErrStr := ERR_AS_11_01;
      $02: ErrStr := ERR_AS_11_02;
      $05: ErrStr := ERR_AS_11_05;
      $06: ErrStr := ERR_AS_11_06;
      $0D: ErrStr := ERR_AS_11_0D;
      $0E: ErrStr := ERR_AS_11_0E;
      $0F: ErrStr := ERR_AS_11_0F;
      $10: ErrStr := ERR_AS_11_10;
      $11: ErrStr := ERR_AS_11_11;
    end;
  end;
  $14: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_14_00;
      $01: ErrStr := ERR_AS_14_01;
    end;
  end;
  $15: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_15_00;
      $01: ErrStr := ERR_AS_15_01;
      $02: ErrStr := ERR_AS_15_02;
    end;
  end;
  $17: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_17_00;
      $01: ErrStr := ERR_AS_17_01;
      $02: ErrStr := ERR_AS_17_02;
      $03: ErrStr := ERR_AS_17_03;
      $04: ErrStr := ERR_AS_17_04;
      $05: ErrStr := ERR_AS_17_05;
      $07: ErrStr := ERR_AS_17_07;
      $08: ErrStr := ERR_AS_17_08;
      $09: ErrStr := ERR_AS_17_09;
    end;
  end;
  $18: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_18_00;
      $01: ErrStr := ERR_AS_18_01;
      $02: ErrStr := ERR_AS_18_02;
      $03: ErrStr := ERR_AS_18_03;
      $04: ErrStr := ERR_AS_18_04;
      $05: ErrStr := ERR_AS_18_05;
      $06: ErrStr := ERR_AS_18_06;
      $08: ErrStr := ERR_AS_18_08;
    end;
  end;
  $1A: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_1A_00;
    end;
  end;
  $1B: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_1B_00;
    end;
  end;
  $1D: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_1D_00;
    end;
  end;
  $20: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_20_00;
    end;
  end;
  $21: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_21_00;
      $01: ErrStr := ERR_AS_21_01;
      $02: ErrStr := ERR_AS_21_02;
    end;
  end;
  $24: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_24_00;
      $01: ErrStr := ERR_AS_24_01;
    end;
  end;
  $25: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_25_00;
    end;
  end;
  $26: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_26_00;
      $01: ErrStr := ERR_AS_26_01;
      $02: ErrStr := ERR_AS_26_02;
      $03: ErrStr := ERR_AS_26_03;
      $04: ErrStr := ERR_AS_26_04;
      $05: ErrStr := ERR_AS_26_05;
      $06: ErrStr := ERR_AS_26_06;
      $07: ErrStr := ERR_AS_26_07;
      $08: ErrStr := ERR_AS_26_08;
      $09: ErrStr := ERR_AS_26_09;
      $0A: ErrStr := ERR_AS_26_0A;
      $0B: ErrStr := ERR_AS_26_0B;
      $0C: ErrStr := ERR_AS_26_0C;
      $0D: ErrStr := ERR_AS_26_0D;
    end;
  end;
  $27: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_27_00;
      $01: ErrStr := ERR_AS_27_01;
      $02: ErrStr := ERR_AS_27_02;
      $03: ErrStr := ERR_AS_27_03;
      $04: ErrStr := ERR_AS_27_04;
      $05: ErrStr := ERR_AS_27_05;
      $06: ErrStr := ERR_AS_27_06;
    end;
  end;
  $28: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_28_00;
      $01: ErrStr := ERR_AS_28_01;
    end;
  end;
  $29: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_29_00;
      $01: ErrStr := ERR_AS_29_01;
      $02: ErrStr := ERR_AS_29_02;
      $03: ErrStr := ERR_AS_29_03;
      $04: ErrStr := ERR_AS_29_04;
      $05: ErrStr := ERR_AS_29_05;
      $06: ErrStr := ERR_AS_29_06;
    end;
  end;
  $2A: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_2A_00;
      $01: ErrStr := ERR_AS_2A_01;
      $02: ErrStr := ERR_AS_2A_02;
      $03: ErrStr := ERR_AS_2A_03;
      $04: ErrStr := ERR_AS_2A_04;
      $05: ErrStr := ERR_AS_2A_05;
    end;
  end;
  $2B: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_2B_00;
    end;
  end;
  $2C: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_2C_00;
      $03: ErrStr := ERR_AS_2C_03;
      $04: ErrStr := ERR_AS_2C_04;
      $06: ErrStr := ERR_AS_2C_06;
    end;
  end;
  $2E: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_2E_00;
    end;
  end;
  $2F: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_2F_00;
    end;
  end;
  $30: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_30_00;
      $01: ErrStr := ERR_AS_30_01;
      $02: ErrStr := ERR_AS_30_02;
      $03: ErrStr := ERR_AS_30_03;
      $04: ErrStr := ERR_AS_30_04;
      $05: ErrStr := ERR_AS_30_05;
      $06: ErrStr := ERR_AS_30_06;
      $07: ErrStr := ERR_AS_30_07;
      $08: ErrStr := ERR_AS_30_08;
      $09: ErrStr := ERR_AS_30_09;
      $10: ErrStr := ERR_AS_30_10;
    end;
  end;
  $31: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_31_00;
      $01: ErrStr := ERR_AS_31_01;
      $02: ErrStr := ERR_AS_31_02;
    end;
  end;
  $34: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_34_00;
    end;
  end;
  $35: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_35_00;
      $01: ErrStr := ERR_AS_35_01;
      $02: ErrStr := ERR_AS_35_02;
      $03: ErrStr := ERR_AS_35_03;
      $04: ErrStr := ERR_AS_35_04;
    end;
  end;
  $37: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_37_00;
    end;
  end;
  $39: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_39_00;
    end;
  end;
  $3A: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_3A_00;
      $01: ErrStr := ERR_AS_3A_01;
      $02: ErrStr := ERR_AS_3A_02;
      $03: ErrStr := ERR_AS_3A_03;
      $04: ErrStr := ERR_AS_3A_04;
    end;
  end;
  $3B: begin
    Case s.AddSenQual of
      $0D: ErrStr := ERR_AS_3B_0D;
      $0E: ErrStr := ERR_AS_3B_0E;
      $0F: ErrStr := ERR_AS_3B_0F;
      $11: ErrStr := ERR_AS_3B_11;
      $12: ErrStr := ERR_AS_3B_12;
      $13: ErrStr := ERR_AS_3B_13;
      $14: ErrStr := ERR_AS_3B_14;
      $15: ErrStr := ERR_AS_3B_15;
      $16: ErrStr := ERR_AS_3B_16;
    end;
  end;
  $3D: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_3D_00;
    end;
  end;
  $3E: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_3E_00;
      $01: ErrStr := ERR_AS_3E_01;
      $02: ErrStr := ERR_AS_3E_02;
      $03: ErrStr := ERR_AS_3E_03;
      $04: ErrStr := ERR_AS_3E_04;
    end;
  end;
  $3F: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_3F_00;
      $01: ErrStr := ERR_AS_3F_01;
      $02: ErrStr := ERR_AS_3F_02;
      $03: ErrStr := ERR_AS_3F_03;
      $04: ErrStr := ERR_AS_3F_04;
      $05: ErrStr := ERR_AS_3F_05;
      $06: ErrStr := ERR_AS_3F_06;
      $07: ErrStr := ERR_AS_3F_07;
      $08: ErrStr := ERR_AS_3F_08;
      $09: ErrStr := ERR_AS_3F_09;
      $0A: ErrStr := ERR_AS_3F_0A;
      $0B: ErrStr := ERR_AS_3F_0B;
      $0C: ErrStr := ERR_AS_3F_0C;
      $0D: ErrStr := ERR_AS_3F_0D;
      $0E: ErrStr := ERR_AS_3F_0E;
      $0F: ErrStr := ERR_AS_3F_0F;
      $10: ErrStr := ERR_AS_3F_10;
      $11: ErrStr := ERR_AS_3F_11;
    end;
  end;
  $43: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_43_00;
    end;
  end;
  $44: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_44_00;
    end;
  end;
  $45: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_45_00;
    end;
  end;
  $46: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_46_00;
    end;
  end;
  $47: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_47_00;
      $01: ErrStr := ERR_AS_47_01;
      $02: ErrStr := ERR_AS_47_02;
      $03: ErrStr := ERR_AS_47_03;
      $04: ErrStr := ERR_AS_47_04;
    end;
  end;
  $48: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_48_00;
    end;
  end;
  $49: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_49_00;
    end;
  end;
  $4A: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_4A_00;
    end;
  end;
  $4B: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_4B_00;
    end;
  end;
  $4C: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_4C_00;
    end;
  end;
  $4E: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_4E_00;
    end;
  end;
  $51: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_51_00;
      $01: ErrStr := ERR_AS_51_01;
    end;
  end;
  $53: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_53_00;
      $02: ErrStr := ERR_AS_53_02;
    end;
  end;
  $55: begin
    Case s.AddSenQual of
      $02: ErrStr := ERR_AS_55_02;
      $03: ErrStr := ERR_AS_55_03;
      $04: ErrStr := ERR_AS_55_04;
    end;
  end;
  $57: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_57_00;
    end;
  end;
  $5A: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_5A_00;
      $01: ErrStr := ERR_AS_5A_01;
      $02: ErrStr := ERR_AS_5A_02;
      $03: ErrStr := ERR_AS_5A_03;
    end;
  end;
  $5B: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_5B_00;
      $01: ErrStr := ERR_AS_5B_01;
      $02: ErrStr := ERR_AS_5B_02;
      $03: ErrStr := ERR_AS_5B_03;
    end;
  end;
  $5D: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_5D_00;
      $01: ErrStr := ERR_AS_5D_01;
      $02: ErrStr := ERR_AS_5D_02;
      $03: ErrStr := ERR_AS_5D_03;
      $FF: ErrStr := ERR_AS_5D_FF;
    end;
  end;
  $5E: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_5E_00;
      $01: ErrStr := ERR_AS_5E_01;
      $02: ErrStr := ERR_AS_5E_02;
      $03: ErrStr := ERR_AS_5E_03;
      $04: ErrStr := ERR_AS_5E_04;
    end;
  end;
  $63: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_63_00;
      $01: ErrStr := ERR_AS_63_01;
    end;
  end;
  $64: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_64_00;
      $01: ErrStr := ERR_AS_64_01;
    end;
  end;
  $65: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_65_00;
    end;
  end;
  $6F: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_6F_00;
      $01: ErrStr := ERR_AS_6F_01;
      $02: ErrStr := ERR_AS_6F_02;
      $03: ErrStr := ERR_AS_6F_03;
      $04: ErrStr := ERR_AS_6F_04;
      $05: ErrStr := ERR_AS_6F_05;
    end;
  end;
  $72: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_72_00;
      $01: ErrStr := ERR_AS_72_01;
      $02: ErrStr := ERR_AS_72_02;
      $03: ErrStr := ERR_AS_72_03;
      $04: ErrStr := ERR_AS_72_04;
      $05: ErrStr := ERR_AS_72_05;
    end;
  end;
  $73: begin
    Case s.AddSenQual of
      $00: ErrStr := ERR_AS_73_00;
      $01: ErrStr := ERR_AS_73_01;
      $02: ErrStr := ERR_AS_73_02;
      $03: ErrStr := ERR_AS_73_03;
      $04: ErrStr := ERR_AS_73_04;
      $05: ErrStr := ERR_AS_73_05;
      $06: ErrStr := ERR_AS_73_06;
    end;
  end;
end;

