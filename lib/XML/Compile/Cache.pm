# Copyrights 2008 by Mark Overmeer.
#  For other contributors see ChangeLog.
# See the manual pages for details on the licensing terms.
# Pod stripped from pm file by OODoc 1.05.
use warnings;
use strict;

package XML::Compile::Cache;
use vars '$VERSION';
$VERSION = '0.13';

use base 'XML::Compile::Schema';

use Log::Report 'xml-compile-cache', syntax => 'SHORT';

use XML::Compile::Util   qw/pack_type unpack_type/;
use List::Util           qw/first/;


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

    $self->{XCC_prefix}  = $self->_namespaceTable(delete $args->{prefixes});

    if(my $anyelem = $args->{any_element})
    {   if($anyelem eq 'ATTEMPT')
        {   push @{$self->{XCC_ropts}}
              , any_element => sub {$self->_convertAnyElementReader(@_)};
        }
    }

    $self;
}

#----------------------


sub prefixes(@)
{   my $self = shift;
    my $p    = $self->{XCC_prefix};
    while(@_)
    {   my ($prefix, $ns) = (shift, shift);
        $p->{$ns} = { uri => $ns, prefix => $prefix, used => 0 };
    }
    $p;
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

    my @opts = $self->_merge_opts
     ( {prefixes => $self->prefixes}
     , $self->{XCC_opts}, $self->{XCC_ropts}
     , \@_
     );

    $self->compile(READER => $type, @opts);
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
    my @opts = $self->_merge_opts
      ( {prefixes => $self->prefixes}
      , $self->{XCC_opts}, $self->{XCC_wopts}
      , \@_
      );
    $self->compile(WRITER => $type, @opts);
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
defined $name or panic "findName";

    my ($type, $ns, $local);
    if($name =~ m/^\{/)
    {   $type = $name;
    }
    else
    {   (my $prefix,$local) = $name =~ m/^(\w*)\:(\S+)$/ ? ($1,$2) : ('',$name);
        my $p  = $self->{XCC_prefix};
        my $ns = first { $p->{$_}{prefix} eq $prefix } keys %$p;
        defined $ns
            or error __x"unknown name prefix for `{name}'", name => $name;

        $type  = pack_type $ns, $local;
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

#---------------
# Convert ANY elements and attributes

sub _convertAnyElementReader(@)
{   my ($self, $type, $nodes, $path, $args) = @_;

    my $reader  = try { $self->reader($type) };
    !$@ && $reader or return ($type => $nodes);

    my @nodes   = ref $nodes eq 'ARRAY' ? @$nodes : $nodes;
    my @convert = map {$reader->($_)} @nodes;
    ($type => (ref $nodes eq 'ARRAY' ? $convert[0] : \@convert));
}


1;
