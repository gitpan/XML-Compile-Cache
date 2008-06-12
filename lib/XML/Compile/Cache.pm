# Copyrights 2008 by Mark Overmeer.
#  For other contributors see ChangeLog.
# See the manual pages for details on the licensing terms.
# Pod stripped from pm file by OODoc 1.05.
use warnings;
use strict;

package XML::Compile::Cache;
use vars '$VERSION';
$VERSION = '0.11';

use base 'XML::Compile::Schema';

use Log::Report 'xml-compile-cache', syntax => 'SHORT';

use XML::Compile::Util   qw/pack_type unpack_type/;


sub init($)
{   my ($self, $args) = @_;
    $self->SUPER::init($args);

    $self->{XCC_opts}   = delete $args->{opts_rw}      || [];
    $self->{XCC_ropts}  = delete $args->{opts_readers} || [];
    $self->{XCC_wopts}  = delete $args->{opts_writers} || [];
    $self->{XCC_undecl} = delete $args->{allow_undeclared} || 0;

    $self->{XCC_rcode}  = {};
    $self->{XCC_wcode}  = {};
    $self->{XCC_dropts} = {};
    $self->{XCC_dwopts} = {};
    $self->{XCC_uropts} = {};
    $self->{XCC_uwopts} = {};

    $self->{XCC_readers} = {};
    $self->{XCC_writers} = {};

    my $prefixes = delete $args->{prefixes}   || [];
    if(ref $prefixes eq 'ARRAY')
    {   $self->{XCC_prefix}  = { @$prefixes };
        unshift @{$self->{XCC_wopts}}, output_namespaces => $prefixes;
    }
    else
    {   $self->{XCC_prefix}  = $prefixes;
        unshift @{$self->{XCC_wopts}}, output_namespaces => [ %$prefixes ];
    }
    $self;
}

#----------------------


sub prefixes(@)
{   my $self = shift;
    while(@_)
    {   my $k = shift;
        $self->{XCC_prefix}{$k} = shift;
    }
    $self->{XCC_prefix};
}


sub allowUndeclared(;$)
{   my $self = shift;
    @_ ? ($self->{XCC_undecl} = shift) : $self->{XCC_undecl};
}

#----------------------


sub compileAll(;$$)
{   my ($self, $need, $usens) = @_;
    my ($need_r, $need_w) = $self->_need($need);

    if($need_r)
    {   while(my($type, $opts) = each %{$self->{XCC_dropts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_rcode}{$type} ||= $self->_createReader($type, @$opts);
        }
    }

    if($need_w)
    {   while(my($type, $opts) = each %{$self->{XCC_dwopts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_wcode}{$type} ||= $self->_createWriter($type, @$opts);
        }
    }
}


sub reader($@)
{   my ($self, $name) = (shift, shift);
    my $type    = $self->findName($name);
    my $readers = $self->{XCC_readers};

    if(exists $self->{XCC_dropts}{$type})
    {   warn __x"ignoring options to pre-declared reader {name}"
          , name => $name if @_;

        return $readers->{$type}
            if $readers->{$type};
    }
    elsif($self->{XCC_undecl})
    {   if(my $ur = $self->{XCC_uropts}{$type})
        {   my $differs = @$ur != @_;
            unless($differs)
            {   for(my $i=0; $i<@$ur; $i++)
                {   $differs++ if $ur->[$i] ne $_[$i];
                }
            }

            error __x"undeclared reader {name} used with different options: before ({was}), now ({this})"
              , name => $name, was => $ur, this => \@_
                  if $differs;
        }
        else
        {   $self->{XCC_uropts}{$type} = \@_;
        }
    }
    elsif(exists $self->{XCC_dwopts}{$type})
    {   error __x"type {name} is only declared as writer", name => $name;
    }
    else
    {   error __x"type {name} is not declared", name => $name;
    }

    $readers->{$type} ||= $self->_createReader($type, @_);
}

sub _createReader($@)
{   my ($self, $type) = (shift, shift);
    trace "create reader for $type";

    $self->compile
     ( READER => $type
     , $self->_merge_opts($self->{XCC_opts}, $self->{XCC_ropts}, \@_)
     );
}


sub writer($)
{   my ($self, $name) = (shift, shift);
    my $type = $self->findName($name);
    my $writers = $self->{XCC_writers};

    if(exists $self->{XCC_dwopts}{$type})
    {   warn __x"ignoring options to pre-declared writer {name}"
          , name => $name if @_;

        return $writers->{$type}
            if $writers->{$type};
    }
    elsif($self->{XCC_undecl})
    {   if(my $ur = $self->{XCC_uwopts}{$type})
        {   my $differs = @$ur != @_;
            unless($differs)
            {   for(my $i=0; $i<@$ur; $i++)
                {   $differs++ if $ur->[$i] ne $_[$i];
                }
            }

            error __x"undeclared writer {name} used with different options: before ({was}), now ({this})"
              , name => $name, was => $ur, this => \@_
                  if $differs;
        }
        else
        {   $self->{XCC_uwopts}{$type} = \@_;
        }
    }
    elsif(exists $self->{XCC_dropts}{$type})
    {   error __x"type {name} is only declared as reader", name => $name;
    }
    else
    {   error __x"type {name} is not declared", name => $name;
    }

    $writers->{$type} ||= $self->_createWriter($type, @_);
}

sub _createWriter($)
{   my ($self, $type) = @_;
    
    trace "create writer for $type";
    $self->compile
     ( WRITER => $type
     , $self->_merge_opts($self->{XCC_opts}, $self->{XCC_wopts}, \@_)
     );
}

# Create a list with options, from a list of ARRAYs and HASHES.  The last
# ARRAY or HASH with a certain option value overwrites all previous values.

sub _merge_opts(@)
{   my $self = shift;
    map { !defined $_ ? () : ref $_ eq 'ARRAY' ? @$_ : %$_ } @_;
}

sub _need($)
{    $_[1] eq 'READER' ? (1,0)
   : $_[1] eq 'WRITER' ? (0,1)
   : $_[1] eq 'RW'     ? (1,1)
   : error __x"use READER, WRITER or RW, not {dir}", dir => $_[1];
}

#----------------------


sub declare($$@)
{   my ($self, $need, $names, @opts) = @_;
    my $opts = @opts==1 ? shift @opts : \@opts;
    $opts = [ %$opts ] if ref $opts eq 'HASH';

    my ($need_r, $need_w) = $self->_need($need);

    foreach my $name (ref $names eq 'ARRAY' ? @$names : $names)
    {   my $type = $self->findName($name);
        if($need_r)
        {   defined $self->{XCC_dropts}{$type}
               and warning __x"reader {name} declared again", name => $name;
            $self->{XCC_dropts}{$type} = $opts;
        }

        if($need_w)
        {   defined $self->{XCC_dwopts}{$type}
               and warning __x"writer {name} declared again", name => $name;
            $self->{XCC_dwopts}{$type} = $opts;
        }
    }

    $self;
}


sub findName($)
{   my ($self, $name) = @_;
    my ($type, $ns, $local);
    if($name =~ m/^\{/)
    {   ($type, $ns, $local) = ($name, unpack_type $name);
    }
    elsif($name =~ m/^(\w+)\:(\S+)$/)
    {   (my $prefix, $local) = ($1, $2);
        $ns = $self->{XCC_prefix}{$prefix}
            or error __x"unknown protocol name prefix `{prefix}'"
                 , prefix => $prefix;

        $type  = pack_type $ns, $local;
    }
    else
    {   error __x"protocol name must show namespace or prefix";
    }

    $type;
}


sub printIndex(@)
{   my $self = shift;
    my $fh   = @_ % 2 ? shift : select;
    my %args = @_;
    my $decl = exists $args{show_declared} ? delete $args{show_declared} : 1;

    return $self->SUPER::printIndex($fh, %args)
        unless $decl;

    my $output = '';
    open my($out), '>', \$output;

    $self->SUPER::printIndex($out, %args);

    close $out;
    my @output = split /(?<=\n)/, $output;
    my $ns     = '';
    foreach (@output)
    {   $ns = $1 if m/^namespace\:\s+(\S+)/;
        my $local = m/^\s+(\S+)\s*$/ ? $1 : next;
        my $type  = pack_type $ns, $local;

        substr($_, 1, 1)
          = $self->{XCC_readers}{$type} ? 'R'
          : $self->{XCC_dropts}{$type}  ? 'r' : ' ';

        substr($_, 2, 1)
          = $self->{XCC_writers}{$type} ? 'W'
          : $self->{XCC_dwopts}{$type}  ? 'w' : ' ';
    }
    print $fh @output;
}

1;
