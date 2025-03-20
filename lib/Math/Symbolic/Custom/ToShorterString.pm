package Math::Symbolic::Custom::ToShorterString;

use 5.006;
use strict;
use warnings;
no warnings 'recursion';

=pod

=encoding utf8

=head1 NAME

Math::Symbolic::Custom::ToShorterString - Shorter string representations of Math::Symbolic trees

=head1 VERSION

Version 0.2

=cut

our $VERSION = '0.2';

use Math::Symbolic qw(:all);
use Math::Symbolic::Custom::Base;

BEGIN {*import = \&Math::Symbolic::Custom::Base::aggregate_import}
   
our $Aggregate_Export = [qw/to_shorter_infix_string/];

# define ln in the parser as the natural logarithm 
use Math::SymbolicX::ParserExtensionFactory (
    ln => sub {
        my $arg = shift;
        return Math::Symbolic::Operator->new('log', Math::Symbolic::Constant->euler(), $arg);
    },
);

use Carp;

=pod

=head1 SYNOPSIS

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

=head1 DESCRIPTION

Provides C<to_shorter_infix_string()> through the Math::Symbolic module extension class. "to_shorter_infix_string()" attempts to provide a string representation of a Math::Symbolic tree that is shorter and therefore more readable than the existing (infix) C<to_string()> method. 

The "to_string()" method wraps every branch in parentheses/brackets, which makes larger expressions difficult to read. "to_shorter_infix_string()" tries to determine whether parentheses are required and omits them. One of the goals of this module is that the output string should parse to a Math::Symbolic tree that is (at least numerically) equivalent to the original expression - even if the resulting Math::Symbolic tree might not be completely identical to the original (for that, use "to_string()"). Where appropriate, it produces strings containing the Math::Symbolic parser aliases C<sqrt()> and C<exp()>.

From v0.2, the module uses L<Math::SymbolicX::ParserExtensionFactory> to automatically add C<ln(x)> as an alias for C<log(e,x)> in the parser, and uses it for string output as well (in the same way as C<sqrt()> and C<exp()>). 

The "to_shorter_infix_string()" does not replace the "to_string()" method, it has to be called explicitly.

=cut

sub to_shorter_infix_string {
    my ($t, $brackets_on) = @_;

    if ( ($t->term_type() == T_CONSTANT) || ($t->term_type() == T_VARIABLE) ) {
        return $t->to_string();
    }

    $brackets_on = 1 unless defined $brackets_on;

    if ( $brackets_on ) {
        
        # check if we can turn brackets off for the tree below
        if ( _is_all_operator($t, B_PRODUCT) || _is_all_operator($t, B_SUM) ) {
            $brackets_on = 0;
        }
        # "expanded" for a simple expression essentially defined as no +/- below a * in the tree
        if ( _is_all_operator($t, [B_SUM, B_DIFFERENCE, B_PRODUCT]) && _is_expanded($t) ) {
            $brackets_on = 0;
        }
    }

    # at this point the top of $t must be an operator
    my $string = '';
    my $op_info = $Math::Symbolic::Operator::Op_Types[$t->type()];
    my $op_str = $op_info->{infix_string};

    if ( $t->arity() == 2 ) {

        # handle special cases
        # prefix operator
        if ( not defined $op_str ) {

            # write ln(x) instead of log(e, x)
            if ( ($op_info->{prefix_string} eq 'log') && ($t->op1()->term_type() == T_CONSTANT) && ($t->op1()->{special} eq 'euler') ) {

                $string .= "ln(" . to_shorter_infix_string($t->op2(), $brackets_on) . ")";
            }
            else { 

                $string .= $op_info->{prefix_string} . "(";
                $string .= join( ', ',
                    map { to_shorter_infix_string($_, $brackets_on) } @{ $t->{operands} } );
                $string .= ')';
            }
        }       
        # 'sqrt' and 'exp' are in the parser, use them
        elsif ( $t->type() == B_EXP ) {

            if ( ($t->op2()->term_type() == T_CONSTANT) && ($t->op2()->value() == 0.5) ) {

                $string .= "sqrt(" . to_shorter_infix_string($t->op1(), $brackets_on) . ")";
            }
            elsif ( ($t->op2()->term_type() == T_OPERATOR) && ($t->op2()->type() == B_DIVISION) &&
                    ($t->op2()->op1()->term_type == T_CONSTANT) && ($t->op2()->op1()->value() == 1) &&
                    ($t->op2()->op2()->term_type == T_CONSTANT) && ($t->op2()->op2()->value() == 2) ) {
                    
                $string .= "sqrt(" . to_shorter_infix_string($t->op1(), $brackets_on) . ")";
            }
            elsif ( ($t->op1()->term_type() == T_CONSTANT) && ($t->op1()->{special} eq 'euler') ) {

                $string .= "exp(" . to_shorter_infix_string($t->op2(), $brackets_on) . ")";
            }
        }
        
        if ( $string eq '' ) {
            # various conditions for temporarily disabling brackets
            my @brackets = ($brackets_on, $brackets_on);

            if ( $brackets_on ) {

                foreach my $i (0,1) {
                
                    my $op = $t->{operands}[$i];
                    
                    if ( ($op->term_type() == T_CONSTANT) || ($op->term_type() == T_VARIABLE) ) {
                        # it's a constant or a variable
                        $brackets[$i] = 0;      
                    }
                    elsif ( $op->term_type() == T_OPERATOR ) {
                        # it's going to be a prefix operator (e.g. sin)
                        if ( !defined($Math::Symbolic::Operator::Op_Types[$op->type()]->{infix_string}) ) {
                            $brackets[$i] = 0;
                        }
                        # remove brackets around exponentiation operator ^
                        elsif ( $op->type() == B_EXP ) {
                            $brackets[$i] = 0;
                        }
                    }
                }
            }

            # keep the spaces around * for readability when removing brackets around exponents
            $op_str = " $op_str " unless ($op_str eq '^') || ( ($op_str eq '*') && !(($t->op1()->term_type() == T_OPERATOR) && ($t->op1()->type() == B_EXP)) );
            
            $string .=
                ( $brackets[0] ? '(' : '' )
              . to_shorter_infix_string($t->op1(), $brackets_on)
              . ( $brackets[0] ? ')' : '' )
              . $op_str
              . ( $brackets[1] ? '(' : '' )
              . to_shorter_infix_string($t->op2(), $brackets_on)
              . ( $brackets[1] ? ')' : '' );
        }
    }
    elsif ( $t->arity() == 1 ) {
        # force brackets around the contents of prefix/function-style operators
        if ( not defined $op_str ) {
            $string .= $op_info->{prefix_string} . "(" . to_shorter_infix_string($t->op1(), 1) . ")";
        }
        else {
            my $is_op1 = $t->op1()->term_type() == T_OPERATOR;

            $string .= "$op_str"
              . ( $is_op1 ? '(' : '' )
              . to_shorter_infix_string($t->op1(), 1)
              . ( $is_op1 ? ')' : '' );
        }
    }
    else {
        carp("Cannot proceed deeper with operator using unsupported number of arguments: " . $t->arity());           
    }

    return $string;
}

# _is_all_operator
# returns 1 if the passed in tree $t is comprised entirely of the
# operator(s) specified in $op_type (excluding prefix-only operators)
sub _is_all_operator {
    my ($t, $op_type) = @_;
    
    return 1 if ($t->term_type() == T_CONSTANT) || ($t->term_type() == T_VARIABLE);

    # this will stop descent into e.g. sin, cos
    my $op = $Math::Symbolic::Operator::Op_Types[$t->type()];
    if ( defined($op->{prefix_string}) and not defined($op->{infix_string}) ) {
        return 1;
    }
    
    if ( ref($op_type) eq "ARRAY" ) {
        my @m = grep { $_ == $t->type() } @{$op_type};    
        return 0 if scalar(@m) == 0;
    }
    else {
        return 0 if $t->type() != $op_type;
    }
    
    my $ok = 1;
    $ok &= _is_all_operator($_, $op_type) for @{$t->{operands}};
    return $ok;
}

# _is_expanded
# returns 1 if there are no +/- below a * in the tree.
# FIXME: Cannot really be run by itself - has to be restricted to the operators involved, i.e.:
# _is_all_operator($t, [B_SUM, B_DIFFERENCE, B_PRODUCT]) && _is_expanded($t)
sub _is_expanded {
    my ($t, $flag) = @_;
    
    $flag = 0 unless defined $flag;

    return 1 if ($t->term_type() == T_CONSTANT) || ($t->term_type() == T_VARIABLE);
    
    my $op = $Math::Symbolic::Operator::Op_Types[$t->type()];
    if ( defined($op->{prefix_string}) and not defined($op->{infix_string}) ) {
        return 1;
    }
    
    if ( $flag && (($t->type() == B_SUM) || ($t->type() == B_DIFFERENCE)) ) {
        return 0;
    }

    if ( ($t->type() == B_PRODUCT) || ($t->type() == B_DIFFERENCE) ) {
        $flag = 1;
    }

    my $ok = 1;
    $ok &= _is_expanded($_, $flag) for @{$t->{operands}};
    return $ok;
}

=pod

=head1 SEE ALSO

L<Math::Symbolic>

=head1 AUTHOR

Matt Johnson, C<< <mjohnson at cpan.org> >>

=head1 ACKNOWLEDGEMENTS

Steffen Mueller, author of Math::Symbolic

=head1 LICENSE AND COPYRIGHT

This software is copyright (c) 2024 by Matt Johnson.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

1;
__END__

