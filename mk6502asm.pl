%instructions = (
              ORA => ["000", \%addr_a],
              AND => ["001", \%addr_a],
              EOR => ["010", \%addr_a],
              ADC => ["011", \%addr_a],
              STA => ["100", \%addr_a],
              LDA => ["101", \%addr_a],
              CMP => ["110", \%addr_a],
              SBC => ["111", \%addr_a],
              ASL => ["000", \%addr_b],
              ROL => ["001", \%addr_b],
              LSR => ["010", \%addr_b],
              ROR => ["011", \%addr_b],
              STX => ["100", \%addr_b],
              LDX => ["101", \%addr_b],
              DEC => ["110", \%addr_b],
              INC => ["111", \%addr_b],
              BIT => ["001", \%addr_c],
              JMP => ["010", \%addr_c],
              JMPA=> ["011", \%addr_c],
              STY => ["100", \%addr_c],
              LDY => ["101", \%addr_c],
              CPY => ["110", \%addr_c],
              CPX => ["111", \%addr_c],
              BPL => ["00010000", undef],
              BMI => ["00110000", undef],
              BVC => ["01010000", undef],
              BVS => ["01110000", undef],
              BCC => ["10010000", undef],
              BCS => ["10110000", undef],
              BNE => ["11010000", undef],
              BEQ => ["11110000", undef],
              BRK => ["00000000", undef],
              JSR => ["00100000", undef],
              RTI => ["01000000", undef],
              RTS => ["01100000", undef],
              PHP => ["00001000", undef],
              PLP => ["00101000", undef],
              PHA => ["01001000", undef],
              DEY => ["10001000", undef],
              TAY => ["10101000", undef],
              INY => ["11001000", undef],
              INX => ["11101000", undef],
              CLC => ["00011000", undef],
              SEC => ["00111000", undef],
              CLI => ["01011000", undef],
              SEI => ["01111000", undef],
              TYA => ["10011000", undef],
              CLV => ["10111000", undef],
              CLD => ["11011000", undef],
              SED => ["11111000", undef],
              TXA => ["10001010", undef],
              TXS => ["10011010", undef],
              TAX => ["10101010", undef],
              TSX => ["10111010", undef],
              DEX => ["11001010", undef],
              NOP => ["11101010", undef]
          );

%addr_a = (
              "INDR" => ["000", \%opcode_a],
              "ZRP"  => ["001", \%opcode_a],
              IMM  => ["010", \%opcode_a],
              "ABS"  => ["011", \%opcode_a],
              "INDI" => ["100", \%opcode_a],
              "INDZ" => ["101", \%opcode_a],
              "ABSY" => ["110", \%opcode_a],
              "ABSX" => ["111", \%opcode_a]
             );

%addr_b = (
              "IMM"  => ["000", \%opcode_b],
              "ZRP"  => ["001", \%opcode_b],
              "ACC"  => ["010", \%opcode_b],
              "ABS"  => ["011", \%opcode_b],
              "INDZ" => ["101", \%opcode_b],
              "ABSX" => ["111", \%opcode_b],
             );

%addr_c = (
              "IMM"  => ["000", \%opcode_c],
              "ZRP"  => ["001", \%opcode_c],
              "ABS"  => ["011", \%opcode_c],
              "INDZ" => ["101", \%opcode_c],
              "ABSX" => ["111", \%opcode_c],
             );

# Compute the final field from existing hashes.
foreach $mode (keys %addr_a)
{
    $opcode_a{$mode} = ["01", undef];
}

foreach $mode (keys %addr_b)
{
    $opcode_b{$mode} = ["10", undef];
}

foreach $mode (keys %addr_c)
{
    $opcode_c{$mode} = ["00", undef];
}

while (<>)
{
    $line++;
    chomp;
    s/[^0-9a-zA-Z:]//g;
    ($mnemonic, $mode) = split/:/;
    exists $instructions{$mnemonic}|| die("Illegal instruction: $mnemonic, line $line\n");

   $head = $instructions{$mnemonic};
   ($opcode, $fr) = ($head->[0], $head->[1]);
   while ($fr)
  {
       exists $fr->{$mode}|| die("Illegal addressing mode $mode for instruction $mnemonic, line $line\n");
       $branch = $fr->{$mode};
       ($fs, $fr) = ($branch->[0], $branch->[1]);
       $opcode .= $fs;
   }
   print "$line\t $mnemonic, $mode\t $opcode\n";
}



