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
              INDX => ["000", \%opcode_a],
              ZRP  => ["001", \%opcode_a],
              IMM  => ["010", \%opcode_a],
              ABS  => ["011", \%opcode_a],
              INDY => ["100", \%opcode_a],
              ZPX  => ["101", \%opcode_a],
              ABSY => ["110", \%opcode_a],
              ABSX => ["111", \%opcode_a]
             );

%addr_b = (
              IMM  => ["000", \%opcode_b],
              ZRP  => ["001", \%opcode_b],
              ACC  => ["010", \%opcode_b],
              ABS  => ["011", \%opcode_b],
              ZPX  => ["101", \%opcode_b],
              ABSX => ["111", \%opcode_b],
             );

%addr_c = (
              IMM  => ["000", \%opcode_c],
              ZRP  => ["001", \%opcode_c],
              ABS  => ["011", \%opcode_c],
              ZPX  => ["101", \%opcode_c],
              ABSX => ["111", \%opcode_c],
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

# Address mode patterns
$hexpat = "[0-9a-fA-F]";
%addr_modes = (
        IMM => ["^\\#\\\$".$hexpat."{2}\$"],
        ZRP => ["^\\\$".$hexpat."{2}\$"],
        ZPX => ["^\\\$".$hexpat."{2}\$", "^X\$"],
        ABS => ["^\\\$".$hexpat."{4}\$"],
        ABSX=> ["^\\\$".$hexpat."{4}\$", "^X\$"],
        ABSY=> ["^\\\$".$hexpat."{4}\$", "^Y\$"],
        INDX=> ["^\\(\\\$".$hexpat."{2},X\\)\$"],
        INDY=> ["^\\(\\\$".$hexpat."{2}\\)\$", "^Y\$"],
        INDR=> ["^\\(\\\$".$hexpat."{4}\\)\$"],
        ACC => ["^A"] );

while (<>)
{
    @argv = ();
    $line++;
    chomp;
# tokenise the input line.
    m/(?<LABEL>\w+(?=:))?:?\s*?(?<INSTR>\w{3,4})\s*(?<A1>(\#?\$?[a-fA-F0-9]{2,4})|\w+|A|\(.+?\))?,? *(?<A2>\$?([a-fA-F0-9]{2,4})|\w+|[XY])?/ || die("Syntax error. Line $line\n");

    ($a1, $a2, $instr, $label) = ($+{A1}, $+{A2}, $+{INSTR}, $+{LABEL});
    if($a1)
    {
        push @argv, $a1;
    }
    if($a2)
    {
        push @argv, $a2;
    }
    
    $argc = scalar @argv;
#    print "Line: $line ($_)\n";
#    print "$argc arguments\n";
#    foreach $key (keys %+)
#    {
#        print "$key: $+{$key}\n";
#    }
#    print "Address mode: ";
    if ($argc == 0)
    {
        $mode = "IMP";
#        print "Implied addressing\n";
    }
    else
    {
        $match = 0;
        $mode = "";
        foreach $key (keys %addr_modes)
        {
            next if $match == $argc;
            $match = 0;
#print "Testing $key\n";
            if (@{$addr_modes{$key}} == $argc) # First check, filters out processing any modes that have a different arg count
            {
                $arg = 0;
                while ($arg < $argc)
                {
#print "Testing $argv[$arg] against $addr_modes{$key}->[$arg], argument $arg\n";
                    if($argv[$arg] =~ m/$addr_modes{$key}->[$arg]/)
                    {
#                       print "Matched\n";
                        $match++;
                        $mode = $key;
                    }
                    else
                    {
#                       print "Not matched\n";
                    }
                    $arg++;
                }
            }
        }
#        if ($match == $argc) # we have a winner Kay
#        {
#            print $mode .$/;
#        }
#        else
#        {
#            die "No matching address mode found\n";
#        }
    }

#    print $/;
    exists $instructions{$instr}|| die("Illegal instruction: $instr, line $line\n");
    if ($label) # Check if the label is known, throw a redefinition error if so, or otherwise add its location to the symbol table
    {
        if(exists $symbol_table{$label}) {
            die("Redefinition of symbol $label, line $line.\n");
        }
        else {
            $symbol_table{$label} = $byte_loc;
        }
    }

   $head = $instructions{$instr};
   ($opcode, $fr) = ($head->[0], $head->[1]);
   while ($fr)
  {
       exists $fr->{$mode}|| die("Illegal addressing mode $mode for instruction $instr, line $line\n");
       $branch = $fr->{$mode};
       ($fs, $fr) = ($branch->[0], $branch->[1]);
       $opcode .= $fs;
   }
   $asm = sprintf("%02x", ord(pack("B8",$opcode)));
   foreach $arg (@argv)
   {
       if ($arg =~ /\#?\$?($hexpat)/)
       {
           $asm .= $1;
       }
   }
   print "$line\t$opcode\t$instr\t$a1\t$a2\t$mode\t";
   print $asm;
   print $/;
   
}



