use strict;
use Math::Symbolic 0.613 qw(:all);
use Math::Symbolic::Custom::ToShorterString 0.2;

# Note: ToShorterString v0.2 automatically adds ln(x) as an alias for log(e,x) in the parser
my $f = parse_from_string("1*2+3*4+5*sqrt(x+y+z)+ln(y)");

# Try displaying with Math::Symbolic's to_string()
my $to_string = $f->to_string();
print "to_string():\t$to_string\n";
# to_string():	(((1 * 2) + (3 * 4)) + (5 * (((x + y) + z) ^ 0.5))) + (log(2.71828182845905, y))

# Try displaying with ToShorterString
my $to_shorter_infix_string = $f->to_shorter_infix_string();
print "to_shorter_infix_string():\t$to_shorter_infix_string\n";
# to_shorter_infix_string():	((1*2 + 3*4) + (5*sqrt(x + y + z))) + ln(y)

# Check that the two output string representations parse to the same expression
my $f2 = parse_from_string($to_string);
my $f3 = parse_from_string($to_shorter_infix_string);

if ( $f2->to_string() eq $f3->to_string() ) {
    print "Parsed to same string\n";
}


