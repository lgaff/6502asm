use feature "switch"; 
use POSIX qw (ceil);

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
};

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
# This is in all honesty a superfluous and pointless operation. I've kept it in for now for clarity in separating opcode fields,
# but it may be taken out later for performance considerations.
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



###############################################################################
################################# SUBROUTINES #################################
###############################################################################
# Tokenise the line
sub tokenise 
{
    my $line = shift;
    $tok  = "";
    @chars = (split(//, $line));
	my @tokens;
TOK:while (@chars)
    {
		while(1) {
			$char = shift @chars;
			last if ($char !~ /[\h]/);

		}
		$match = 1;
		$tok = $char;
		while ($match)
		{
			$tokref = undef;
			given ($tok)
			{
				when (/[\n;]|[^\w\h,\+\-\=:\(\)\"\'\!\@\#\$\%\^\&\*\<\>\.\/\\\?]/)	{ return @tokens }
				when (/,/) { $tokref = ["COMMA", undef] }
				when (/\+/) { $tokref = ["PLUS", undef] }
				when (/\-/) { $tokref = ["MINUS", undef] }
				when (/:/)  { $tokref = ["COLON", undef] }
				when (/\=/) { $tokref = ["EQ", undef] }
				when (/\(/) { $tokref = ["LBRACE", undef] }
				when (/\)/) { $tokref = ["RBRACE", undef] }
				when (/\"/) 
				{
					$tok = "";
					$char = shift @chars;
					QUOT: {
						while ($char !~ /\\|\"|\n/) 
						{
							$tok .= $char;
							$char = shift @chars;
						}
						given ($char)
						{
							when (/\\/) 
							{
								$char = shift @chars;
								$tok .= $char;
								$char = shift @chars;
								redo QUOT;
							}
							when (/\"/) { $tokref = ["QUOTEDSTRING", $tok] }
							default { return }
						}
					}

				}
				when (/[\h\t]/) { return  } # unmatched token
				when (/^[a-zA-Z_]$/) 
				{ 
					# Look ahead until we either find an opcode, label or nothing.
					while (($char = shift @chars) =~ /[a-zA-Z0-9_]/) { $tok .= $char }
					given ($tok)
					{
						when (exists($instructions{uc$_})) { $tokref = ["OPCODE", $_] }
						when (/^[XYA]$/i) { $tokref = ["REG", $_] }
						default  { $tokref = ["LABEL", $_] }
					}
					unshift @chars, split(//, $char);
				}
				when (/#/)	
				{ 
					$char = shift @chars;
					$tok .= $char;
					given ($char)
					{
						when (/\$/)
						{
							while (($char = shift @chars) =~ /$hexpat/) { $tok .= $char }
							$tokref = ["IMM", $tok];
						}
						when (/\d/)
						{
							while (($char = shift @chars) =~ /\d/) { $tok .= $char }
							$tokref = ["IMM", $tok];
						}
						when (/[a-zA-Z_]/)
						{
							while (($char = shift @chars) =~ /\w/) { $tok .= $char }
							$tokref = ["IMM", $tok];
						}
						default { return }
					}
					unshift @chars, split(//, $char);
				}
				when (/\$/)	
				{
					$char = shift @chars;
					given ($char)
					{
						when (/$hexpat/)
						{
							$tok = $char;
							while (($char = shift @chars) =~ /$hexpat/) { $tok .= $char }
							$tokref = ["HEX", $tok];
						}
						when (/\-|\+/)
						{
							$tokref = ["DOLLAR", undef];
						}
						default { return }
					}
					unshift @chars, split(//, $char);
				}
				when (/^\d|[1-9]\d|1[01]\d|12[0-7]$/) { $tokref = ["INT", $tok] } # Integers
				when (/\./)	
				{ 
					$tok = "";
					while (($char = shift @chars) =~ /[a-zA-Z]/) { $tok .= $char }
					$tokref = ["DEF", $tok]; 	# definition macros
					unshift @chars, split(//, $char);
				}
				default { $tok .= shift @chars }											# anything else
			}
			if (defined($tokref)) 
			{
				push @tokens, $tokref;
				$tok = "";
				$match = 0;
			}
		}
	}
}


# Pass 1 simply scans for labels and pops the PC value into the symbol table for pass 2.

sub pass_one
{
    my $line = shift;
    my $sentence = "";
    my $bytes = 0;
    for $token (@{$line}) 
    {
        given ($token->[0])
        {
            when (/HEX/) { $bytes += ceil((length($token->[1])/2)) }
            when (/IMM/) { $bytes += 1 }
            when (/INT/) { $bytes += 1 }
            when (/OPCODE/) { $bytes += 1 }
            when (/DEF/) {
                given ($token->[1])
                {
                    when (/byte/) { $bytes += 1 }
                    when (/word/) { $bytes += 2 }
                    when (/org/) { 
                        $ORG = hex $token->[1];
                    }
                }
            }
            when (/QUOTEDSTRING/) { $bytes += length($token->[1]) }
        }
        $sentence .= "$token->[0] " 
    }
    given ($sentence) 
    {
        when (/^LABEL (EQ|COLON)?/) 
        { 
            if (!exists $symbol_table{$line->[0][1]}) { $symbol_table{$line->[0][1]} = $PC }
            else { return }
        }
    }
    $PC += $bytes;
}

    

# Validate the address modes from the input line where necessary
sub parse
{

}



$PC = 0;
$ORG = 0;
while (<>)
{
    $line++;
    @tokens = tokenise($_);
    if (scalar @tokens) { push @codelines, [@tokens] }
    @tokens = [];
}
while ($token_ref = shift @codelines) { pass_one($token_ref) }

for $symbol (keys %symbol_table) { print "$symbol: $symbol_table{$symbol} \n"};
