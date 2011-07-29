# Constant address mode masks
use constant {
    IMMDT   => 0x1,
    ZEROP   => 0x2,
    ZEROX   => 0x4,
    ABSLT   => 0x8,
    ABSLX   => 0x10,
    ABSLY   => 0x20,
    INDRX   => 0x40,
    INDRY   => 0x80,
    INDRC   => 0x100,
    RELTV   => 0x200,
    IMPLD   => 0x400,
    ACMLT   => 0x800
}

%instructions = ( # First part of opcode, pointer to next part, address mode mask.
              ORA => ["000", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              AND => ["001", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              EOR => ["010", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              ADC => ["011", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              STA => ["100", \%addr_a, ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              LDA => ["101", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              CMP => ["110", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              SBC => ["111", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              ASL => ["000", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX|ACMLT],
              ROL => ["001", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX|ACMLT],
              LSR => ["010", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX|ACMLT], 
              ROR => ["011", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX|ACMLT],
              STX => ["100", \%addr_b, ZEROP|ZEROY|ABSLT],
              LDX => ["101", \%addr_b, IMMDT|ZEROP|ZEROY|ABSLT|ABSLY],
              DEC => ["110", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX],
              INC => ["111", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX],
              BIT => ["001", \%addr_c, ZEROP|ABSLT],
              STY => ["100", \%addr_c, ZEROP|ZEROY|ABSLT ],
              LDY => ["101", \%addr_c, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX ],
              CPY => ["110", \%addr_c, IMMDT|ZEROP|ABSLT ],
              CPX => ["111", \%addr_c, IMMDT|ZEROP|ABSLT ],
              JMP => ["01", \%addr_d, ABSLT|INDRC ],
              BPL => ["00010000", undef, RELTV],
              BMI => ["00110000", undef, RELTV],
              BVC => ["01010000", undef, RELTV],
              BVS => ["01110000", undef, RELTV],
              BCC => ["10010000", undef, RELTV],
              BCS => ["10110000", undef, RELTV],
              BNE => ["11010000", undef, RELTV],
              BEQ => ["11110000", undef, RELTV],
              BRK => ["00000000", undef, IMPLD],
              JSR => ["00100000", undef, ABSLT],
              RTI => ["01000000", undef, IMPLD],
              RTS => ["01100000", undef, IMPLD],
              PHP => ["00001000", undef, IMPLD],
              PLP => ["00101000", undef, IMPLD],
              PHA => ["01001000", undef, IMPLD],
              PLA => ["01101000", undef, IMPLD],
              DEY => ["10001000", undef, IMPLD],
              TAY => ["10101000", undef, IMPLD],
              INY => ["11001000", undef, IMPLD],
              INX => ["11101000", undef, IMPLD],
              CLC => ["00011000", undef, IMPLD],
              SEC => ["00111000", undef, IMPLD],
              CLI => ["01011000", undef, IMPLD],
              SEI => ["01111000", undef, IMPLD],
              TYA => ["10011000", undef, IMPLD],
              CLV => ["10111000", undef, IMPLD],
              CLD => ["11011000", undef, IMPLD],
              SED => ["11111000", undef, IMPLD],
              TXA => ["10001010", undef, IMPLD],
              TXS => ["10011010", undef, IMPLD],
              TAX => ["10101010", undef, IMPLD],
              TSX => ["10111010", undef, IMPLD],
              DEX => ["11001010", undef, IMPLD],
              NOP => ["11101010", undef, IMPLD]
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

%addr_d = (
              ABS  => ["001100", undef],
              INDR => ["101100", undef]
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
        IMM => [0,"^\\#\\\$".$hexpat."{2}\$"],
        ZRP => [1,"^\\\$".$hexpat."{2}\$"],
        ZPX => [2,"^\\\$".$hexpat."{2}\$", "^X\$"],
        ABS => [3,"^\\\$".$hexpat."{4}\$"],
        ABSX=> [4,"^\\\$".$hexpat."{4}\$", "^X\$"],
        ABSY=> [5,"^\\\$".$hexpat."{4}\$", "^Y\$"],
        INDX=> [6,"^\\(\\\$".$hexpat."{2},X\\)\$"],
        INDY=> [7,"^\\(\\\$".$hexpat."{2}\\)\$", "^Y\$"],
        INDR=> [8,"^\\(\\\$".$hexpat."{4}\\)\$"],
        REL => [9,"^\*[+-](\d|[1-9][0-9]|1[01][0-9]|12[0-7])\$"],
        IMP => [10,"^\$"],
        ACC => [11,"^A"] );

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



