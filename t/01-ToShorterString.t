use Math::Symbolic qw(:all);
use Math::Symbolic::Custom::ToShorterString;

use Test::More;

my @tests = (
    '1+2+3+4+5',
    '1*2*3*4*5',
    '1*2+3*4+5',
    'y*2*sin(x+y+z)*c',
    '(x+y*2)^0.5 + g + 2',
    'sin(theta*phi*rho)^2 + cos(theta*phi*rho)^2',
    '(x^2 + 4*x + 16)/(3*x^2 + 3*x - 12)',
    '5*x*x*x + 3*x*x + 4*x + 5',
    '3*4',
    '3*5+6*4',
    '2^2',
    '2^9',
    '2*4+3^3',
    '(3+2)*(5+3)',
    '(3+2)^2',
    '1^3+2^3+3^3+3*(1+2+3)',
    '-3*4',
    '-3*5+6*-4',
    '-2^2',
    '-2^9',
    '2*-4+(-3)^3',
    '(-3-2)*(-5-3)',
    '(-3-2)^2',
    '2/1',
    '11/2',
    '-11/-22',
    '-5/10',
    '5/-10',
    '(2/3)-(-1/6)',
    '(2/3)-(1/-6)',
    '(-2/3)-(1/6)',
    '(2/-3)-(1/6)',
    '(-2/-3)-(1/6)',
    '(2/3)-(-1/-6)',
    '24/36',
    '36/30',
    '45/75',
    '2001/3000',
    '2740/8220',
    '(2/4)+(1/4)',
    '(1/2)+(1/4)+(1/4)',
    '(3*3)/(3-1)',
    '(4/5)*(2/3)',
    '(11/2)*2',
    '2*(10/2)',
    '3*((10+12)/6)',
    '(2/3)-(1/6)',
    '(2/4)-(2/8)',
    '(3/10)/(18/25)',
    '(0+(8/13))+(5/13)',
    '9*8*7*6*5*4*3*2*1',
    '9+8+7+6+5+4+3+2+1',
    '9-8-7-6-5-4-3-2-1',
    '9-8+7-6+5-4+3-2+1',
    '9/8/7/6/5/4/3/2/1',
    '9-8*7-6*5-4*3-2*1',
    '9/8+7/6+5/4+3/2+1',
    'x+3',
    '7*y-5',
    'a*b',
    '8/c',
    '1+(-1*a)',
    'a*(b+c)',
    'a*(b-c)',
    '(a+b)^2',
    '(a-b)^2',
    '(a+b)^3',
    '(a-b)^3',
    '3*(a+7)',
    '3*(a-7)',
    '4*((3*a)+b)',
    '(3+a)*(b+7)',
    '(3+a)*(b-7)',
    '(3-a)*(b-7)',
    '(1+a+b)*(2+a+b)',
    '(1+a-b)*(2-b+c)',
    '5*(a+b)^2',
    '-5*(a+b)^2',
    '4*(((3*a)*b)+c)',
    '4*(((3*a)*b)+(2*c)+(3*d))',
    '2*(a+b)*(c+d)',
    '(3-a)*(b-7)*(c-5)',
    '-13*a+5*b+18*a+(-18*b)',
    '(a-2*b)-(-a+5*b)',
    '-2/-x',
    '2/-x',
    '-2/(-2*x)',
    '(a*(b+c))/((b+c)*a)',
    '(6*x+4)/2',
    '(21*w+9)/15',
    '(-12*y+36)/-8',
    '(5*z-30)/-5',
    '(5/(x+1))-((x-2)/(x+1))',
    '(1/(x+1))+(x/(4-x))-(1/(x-2))',
    '(a-a^2)/(3*a^3-3*a)',
    '((1/(3*a))-(1/(3*b)))/((a/b)-(b/a))',
    '2*x^2-4*x+1',
    '3*x^3-2*x^2+x-6',
    '7*x^4-3*x^3+5*x^2-x+10',
    '5*4+3*x^3-2*x^2+x-6*3+2*5',
    '4*a*(b+c)-d',
    '4*a*(b+c)-d+3*7',
    '(3*7)+(n*m)+(3*m)',
    '(-3*7)+(n*m)+(3*m)',
    '(3*7)-(n*m)+(3*m)',
    '(3*-7)+(n*m)-(3*m)',
    '(2+3*7)/(1-5*8)',
    '(5*x+7)/(2*y-1)',
    '(5*x-7)/(2*y+1)',
    '((5*x+7)/(2*y-1))+((5*x-7)/(2*y+1))',
    '((5*x+7)/(2*y-1))-((5*x-7)/(2*y+1))',
    'sqrt(x^2+y^2)',
    '(a+b)^3-(c-d)^2',
    '((3*x^3+4*y^2-5*z)/(2*x-y))+4*2',
    '(a^2+b^2)*(c^2-d^2)',
    '(sqrt(a+b)+sqrt(c-d))/e+f',
    '(sqrt(a^2+b^2)+sqrt(c^2-d^2))/e^2+f^2',
    '((a+b)*(c-d))/sqrt(e^2+f^2)',
    'sin(x)',
    'cos(x)',
    'tan(x)',
    'sin(2*x)+cos(2*x)',
    'tan(x)-sin(x)/cos(x)',
    'sin(x)^2+cos(x)^2',
    '1-sin(2*x)/cos(2*x)',
    'sin(x+y)-cos(x-y)',
    'sin(x)*cos(x)',
    'sin(x)+cos(y)+tan(z)',
    'sin(x)-cos(y)-tan(z)',
    'sin(x+y)+cos(y+z)+tan(x+z)',
    'sin(sqrt(x+y))+cos(sqrt(y+z))+tan(sqrt(x+z))',
    'cos(x)^2-sin(x)^2',
    'sin(3*x)-3*sin(x)',
    'cos(3*x)-4*cos(x)',
    'sin(x)/cos(x)+cos(x)/sin(x)',
    '2*sin(x)*cos(x)',
    'tan(x+y)',
    'cos(x)*tan(x)+sin(x)',
    'cos(x+y)+cos(x-y)',
    '2*cos(x)^2-1',
    'sin(x)^3+cos(x)^3',
    'cos(x)^3-3*cos(x)*sin(x)^2',
    'sin(x/2)+cos(x/2)',
    'sin(x)-tan(x)/sec(x)',
    'cos(x)*sin(2*x)-sin(x)*cos(2*x)',
    'sin(x)^4-cos(x)^4',
    'cos(x)^4-6*cos(x)^2*sin(x)^2+sin(x)^4',
    'sin(x+y+z)',
    'cos(2*x+y)',
    '2*sin(x)*cos(3*x)-3*sin(2*x)',
    'tan(2*x)-2*tan(x)/(1-tan(x)^2)',
    'cos(x)^2-sin(x)^2+2*cos(x)*sin(x)',
    'sinh(x)',
    'cosh(x)',
    'sinh(2*x)+cosh(2*x)',
    'sinh(x)/cosh(x)',
    'sinh(x)^2+cosh(x)^2',
    '1-sinh(2*x)/cosh(2*x)',
    'sinh(x+y)-cosh(x-y)',
    'sinh(x)*cosh(x)',
    'cosh(x)^2-sinh(x)^2',
    'sinh(3*x)-3*sinh(x)',
    'cosh(3*x)-4*cosh(x)',
    'sinh(x)/cosh(x)+cosh(x)/sinh(x)',
    '2*sinh(x)*cosh(x)',
    'cosh(x)*sinh(x)+sinh(x)',
    'cosh(x+y)+cosh(x-y)',
    '2*cosh(x)^2-1',
    'sinh(x)^3+cosh(x)^3',
    'cosh(x)^3-3*cosh(x)*sinh(x)^2',
    'sinh(x/2)+cosh(x/2)',
    'sinh(x)-sinh(x)/cosh(x)',
    'cosh(x)*sinh(2*x)-sinh(x)*cosh(2*x)',
    'sinh(x)^4-cosh(x)^4',
    'sinh(x+y+z)',
    'cosh(2*x+y)',
    '2*sinh(x)*cosh(3*x)-3*sinh(2*x)',
    'sinh(x+y)*cosh(x-y)',
    'cosh(x+y)*cosh(x-y)',
    'sinh(2*x)-2*sinh(x)*cosh(x)',
    'cosh(x)^2-sinh(x)^2+2*cosh(x)*sinh(x)',
    'exp(3)',
    'exp(x)',
    'exp(2*x)',
    'exp(sqrt(3))',
    'exp(3*2+2*1)',
    'exp(sqrt(3*2+2*1))',
    'exp(sin(2*x))',
    '1+exp(x)',
    '1-exp(x)',
    'exp(3)-exp(-2^2)',
    'exp(sin(x))-exp(-sin(x))',
    'exp(cos(x))-exp(-cos(x))',
    'exp(x)+exp(y)',
    'exp(x)-exp(-x)',
    'exp(x+y)',
    'exp(x)*exp(y)',
    'exp(x)/exp(y)',
    'exp(2*x)-exp(-2*x)',
    'exp(x)*(exp(y)+exp(z))',
    'exp(x)^2+exp(y)^2',
    'exp(x)+1/exp(x)',
    'exp(x)-1/exp(x)',
    'exp(x/2)+exp(-x/2)',
    'exp(x+y)/exp(z)',
    'exp(3*x)-3*exp(x)',
    'exp(x+y+z)',
    'exp(x-y)*exp(y-z)',
    'exp(x^2)-exp(-x^2)',
    'exp(x)+exp(y)-exp(z)',
    'log(2.718281828459,5)',
    'log(2,5)+log(3,10)',
    '(2*log(2,3))/(10*log(4,10))',
    '(x*log(2,3))/(y*log(4,10))',
    'log(1+2*3,1+2+3+4)*log(2*1+1*3,3*2+1)',
    'log(3,3)+log(2,2)',
    '(exp(3*x)-3*exp(x))*((a-b)^3)',
    '(2*4+3^3)-(9+8+7+6+5+4+3+2+1)',
    '(cosh(x+y)*cosh(x-y))-((3+2)*(5+3))',
    '(sin(x+y)-cos(x-y))+((a-a^2)/(3*a^3-3*a))',
    '(45/75)*(exp(x)*(exp(y)+exp(z)))',
    '((5/(x+1))-((x-2)/(x+1)))-(2^2)',
    '(cosh(x+y)*cosh(x-y))*((3+a)*(b-7))',
    '(sinh(2*x)+cosh(2*x))-(exp(sin(x))-exp(-sin(x)))',
    '(sinh(x)^4-cosh(x)^4)+(sinh(x)^3+cosh(x)^3)',
    '(2*4)-(9+8+7+6+5+4+3+2+1)',
    '(2*4)-(6-3)',
    '(2*4)-(6*3)',
    '(2*4+3^3)-(9+8+7+6+5+4+3+2+1)',
);


TEST_LOOP: foreach my $test (@tests) {
    my $f = parse_from_string($test);
    # can the parser parse the test string?
    ok( defined($f), "parsing test string [$test]" );
    if (!defined $f) {
        next TEST_LOOP;
    }  

    my $str1 = $f->to_string();
    my $str2 = $f->to_shorter_infix_string();
    
    # does to_shorter_infix_string() actually produce any output?
    ok(defined($str2) && length($str2), "using to_shorter_infix_string() to stringify tree [$test]");

    # output from to_shorter_infix_string() should really be shorter than to_string(), except for very simple expressions
    ok(length($str2) <= length($str1), "checking to_short_infix_string() produces shorter (or equal) length output than to_string() [$str1 : " . length($str1) . " |vs| " . length($str2) . " :  $str2]");
 
    my $f2 = parse_from_string($str2);
    # can the parser parse the string produced by to_shorter_infix_string()?
    ok( defined($f2), "re-parsing output string [$str2]" );
    if (!defined $f2) {
        next TEST_LOOP;
    }  
 
    # the new Math::Symbolic tree should be numerically equivalent to the original tree
    ok($f->test_num_equiv($f2), "re-parsed output string is numerically equivalent to original [$test |vs| $str2]");  
}

# if all that passes, then we can have some confidence the new function is both reliable and useful
done_testing( 5*scalar(@tests) );



