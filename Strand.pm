package Tie::Strand;
require Exporter;
our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw( @MAGICSTORE ) ] );
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw(  );

our $VERSION = '0.01_01';

# this package may be trying to do too much perhaps we should limit
# it's functionality to strictly dealing with the normal and expanded
# views and managing data for these. This probably means that most of the
# `MAGIC_ACTIONS' should be implemented elsewhere, or maybe not ...

our ( %MAGIC_ACTIONS, %magic_actions, @REQUIRED_ACTIONS, @STORAGE );
use Log::Easy qw(:all);
use strict;
no strict 'refs';
use Tie::Silk;
use constant _ARRAYCLASS_ => 'Tie::Silk';
#tie %MAGIC_ACTIONS, _ARRAYCLASS_;
#tie %magic_actions, _ARRAYCLASS_;
# these represent  things  that may be   initialized at instantiation
# which would override the normal process of setting up these values
my @INITIALIZE = qw( CLASS SETTING DEFINE EXPAND INIT );
# for each widget the argument list is as follows
#[
# 0: reserved: widget class
# 1: reserved: argument set to widget { -<ACTION|all> => { -arguments => 'for this ACTION', -or_for => "all ACTION's" }
# 2: reserved: TBD - stowage arguments => stowage column name (STRING or CODE ref) or TRUE to persist
# 3: reserved: TBD - stowage arguments => stowage args (HASH) or TRUE to persist
# 4: reserved: [ qw( expand into these elements in the EXPAND view ) ]
#]

my %SETABLE   = ( # these are settings that are understood by Strand type thingies by default
		 # if CLASS also has settings then those can be used
		 # (the inheritance heierarchy should be searched I guess too?)
                 #setting_name => 'subroutine_to_process_settings'
                 AUTO_VALIDATE => '_set_auto_validation', # validate entries as they are set
                 IMMUTABLE     => '_set_immutable',       # if true, then no-one may alter attributes after instantiation
		 INIT          => '_set_init', # this is here for a sub-class to use
                 WIDGETCLASS   => '_set_widgetclass',
                );
#%define = ( foo => ['bar' => {-all=>{}}],
#            baz => ['bif' => {-all=>{}}],
#          );
%magic_actions  =
  # when you try to STORE or FETCH something with one of these names, the `action' value will be executed
  # for STORE eg $get_user_response = $h{RESPONSE} = baz; will use the widget `bif' to get input from the user and set the `baz' paremeter
  # for STORE eg @get_user_responses = $h{RESPONSE} = 'ARRAY'; will use the entire definition to get input from the user [answers only]
  # for STORE eg %get_user_responses = $h{RESPONSE} = 'HASH'; will use the entire definition to get input from the user(equivalent to the FETCH $h{RESPONSE})
  # for FETCH eg %get_user_responses = $h{RESPONSE}; will use the entire definition to get input from the user(equivalent to the STORE $h{RESPONSE} = 'HASH')
  # so if you want to display a form on which a user can put input, you would say:
  # my $form = $object->{INPUT};
  # if the object had many attripbutes then you could get only certain for elements yb doing this
  # my $text_input = $object->{INPUT}{name}; # single form element
  # my $complex_input = $object->{INPUT}{address}; # multiple form elements
  ( # $self->{DEFINE}
   # utility functions
   INITIALIZE => '_initialize', # initializes all required data structures for the object, needs to be done before object can really be used so always $_{INITIALIZE} immediately after tie %
   # things about the structure of the object we're dealing with
   ATOMIC     => '_atomic'  , # returns false if it is composite, true if atomic
   ATOMICS    => '_atomics' , # return a list of all of the widgets in EXPAND that are atomic [key names]
   DEFINE     => '_define'  , # returns the definition of the object
   EXPAND     => '_expand'  , # returns the expand definition of the object including all intermediate components
   EXPANDED   => '_expanded', # returns the expanded object view which uses EXPAND for a definition instead of DEFINE
   LINT       => '_lint'    , # test widget for consistency
   # basic user interface stuff
   INPUT      => '_input'   , # returns a structure suitable for rendering into a user interface edit screen
   RESPONSE   => '_response', # data entered by a user but potentially not yet approved
   STORE      => '_store'   , # forces merging of RESPONSE data with VALUE data and puts it into PERSIST slot (accept response in other words ... data will be persisted if it is persistent data)
   VERIFY     => '_verify'  , # returns a structure suitable for rendering into a user interface verify screen (post edit, pre commit)
   DISPLAY    => '_display' , # returns a structure suitable for rendering into a user interface view screen (view into data store)
   VALIDATE   => '_validate', # forces validation of RESPONSE data
   ERROR      => '_error'   , # returns all errors associated with current state of the object, expanded view of errors
   INVALID    => '_invalid' , # returns all errors associated with current state of the object, non-expanded view of errors
   PERSIST    => '_persist' , # data retreived from the persistent data store
   TOP        => '_top'     , # the entire data set that this widget represents with its current values
   VALUE      => '_value'   , # data slated to be stored in the persistent data store(queued)
   ROLLBACK   => '_rollback', # abort any changes not yet COMMIT'ed (could we add a rollback to any earlier version/state? ala cvs?)
   COMMIT     => '_commit'  , # write PERSIST data to external data store
   STOW       => '_stow'    , # a map of how the object will be persisted (in a database or what not)
   STRANDS    => '_strands' , # a list of all the widgets represented by this conglomerate <CLASS> => $class->new()
   OPTION_LIST => '_option_list', # this allows you to set an option list to something different than the one described in the calling widget
  );

%MAGIC_ACTIONS = %magic_actions;
# these required subs also translate to required actions ala "${strand}_$action"
@REQUIRED_ACTIONS = qw( edit strand define display verify validate autofill template test parse unparse );
#@REQUIRED_ACTIONS = qw( validate );

@STORAGE = (
	    # these are for the top level values as described in %{"$objclass\::DEFINE"}
	    'PERSIST' , # # top level data as most recently retreived from stowage datastore
	    'DEFINE'  , # # data storage for top level data, usu. a result of munging in EXPAND data, but may be set directly (with cascading effects on EXPAND where necessary)
	    'EXPAND', # # internal working area for data munging (setting a subcomponent value may alter the value of the data that depends on it (super-component), as well as underlying data(sub-component))
	    'RESPONSE', # # parameter data entered by the user (usu. only the atomic elements of EXPAND)
	    'ACCEPT'  , # # top level data bound for stowage datastore(data in DEFINE is accepted - we only commit if there is stuff in ACCEPT)
	    'ERROR'   , # # errors resulting from internal operations
	    'INVALID' , # # errors from validating user input data (if anything is INVALID then the DEFINE top level data will probably NOT be accepted into ACCEPT)
	   );

*TIEARRAY = \&TIEHASH;

# each tied hash gets two ties hashes, one is the regular
# view(parent), and the other is basically hidden from the end
# developer and represents the fully expand view (inside the black
# box - generated through recursion) therefore the parent will accept
# input for the expand view, as well as it's own data fields. if
# there is a field in the expand view that is the same (eg an atomic
# element) as the one in the parent then the expand view placeholder
# NEVER gets used (at least that is how it is now)
sub TIEHASH {
  $log->write($tll, "TRACING: ");
  #$log->write($sll, "TRACING ARGS: ", \@_);
  my $class = shift;
  
  my %init = ();
  #$log->write($sll, "PRE INITIALIZE: ", \%init);
  #$log->write($sll, "PRE INITIALIZE \@_: ", \@_);
 INIT:
  foreach my $init ( @INITIALIZE ) {
    next unless $init;
    $log->write($dll, " $_[0] eq /$init/ ? \$_[1] = ", $_[1]);
    if ( $_[0] and $_[0] eq $init ) {
      if ( 1 or ref $_[1] eq 'HASH' ) {
        $log->write($dll, "INITIALIZING `$init': \$_[0]=", $_[0], ", \$_[1]=", $_[1]);
        $init{$_[0]} = $_[1];
        $log->write($dll, "INITIALIZED: `$init' $_[0]: ", $init{$_[0]});
        shift;shift;
        goto INIT;
      } else {
        $log->write(CRIT, "INITIALIZING `$init' : MUST BE A HASH REFERENCE! \@_ : ", \@_ );
      }
    }
  }
  $log->write($dll, "INITIALIZE: ", \%init);
  my $objclass = $init{CLASS}  or $log->write(CRIT, "CLASS => <widget class> must be specified:", \%init );
  $log->write($dll, '$objclass: ', $objclass);
  my $strand   = $objclass->strand();
  my $wclass   =  $strand =~ /\:\:/ ? $strand  # fully qualified widget
    : $objclass;#"Object::Strand::$strand";      # standard widget
  $log->write($dll, '$wclass: ', $wclass);
  
  my $define   = \%{"$objclass\::tied::DEFINE"};
  my $td = tied %$define;
  my $expand = \%{"$objclass\::tied::EXPAND"};
  my $tx = tied %$expand;
  my $setting  = \%{"$objclass\::tied::SETTING"};
  unless ( scalar keys %$define and ref $td eq _ARRAYCLASS_ ) {
    my $adefine = [ @{"$objclass\::DEFINE"} ];
    unless ( @$adefine ) {
      $log->write($dll, 'This is probably atomic: ', $objclass);
      @$adefine = ( $strand => [ $strand => { -arguments => 'should go here' } ] );
    } else {
      $log->write($dll, 'This is probably composite: ', $objclass );
    }
    $log->write($dll, " $objclass: \$adefine: ",  $adefine);
    $td = tie %$define, _ARRAYCLASS_,  @{$adefine};
    $tx = tie %$expand, _ARRAYCLASS_;
    #bless $expand, 'Tie::Strand::EXPAND';
  }

  my $tc = { #DEFINE      => $define,
	    #TD          => $td,
	    #TE          => $te,
	    #WIDGET      => $$widget,
	    SETTING     => { WIDGETCLASS => $wclass,
                             %$setting },
	    STORAGE     => { map { $_ => [] } @STORAGE },
	    CLASS       => $objclass,
	    STRAND      => $strand,
	   };
  $setting = $tc->{SETTING};
  # make sure any auto validation is done up front as well as on changes
  $tc = bless $tc, $class;
  $log->write($dll, ' \%$tc: ',  \%$tc);
  if ( $init{SETTING} ) {
    $log->write($dll, 'SETTING: ', $init{SETTING});

    my $sub;
    while ( my ( $what, $value ) = each %{$init{SETTING}} ) {
      next unless ($what and $value);
      if ( $sub = $SETABLE{$what} ) {
	$log->write($nll, "$wclass ::: setting internal value $what : $value using $sub => ", $value);
        $setting->{$what} = $tc->$sub( $value );
      } else {
        $log->write(CRIT, "No such setting allowed: `$what' => ", $setting->{$what});
      }
    }
  }

  if ( $init{PARAM} ) {
    while ( my ( $attr, $value ) = each %{$init{PARAM}} ) {
      STORE( $tc, $attr => $value ) if defined $value;
    }
  }
  $log->write($dll, "RETURNING: ", $tc );
  #$log->write({n=>undef}, $sll, "Press return to continue: "), <STDIN>;
  return $tc;
}

our @MAGICSTORE = ();
{
  # this is local'ized later on to help us determine to whence a change propagates, up from here or down
  our $UPORDOWN   = 'NEITHERUPNORDOWN';
  sub STORE {
    $log->write($sll, '@_: ', \@_);
    $log->write($dll, '$UPORDOWN: ', $UPORDOWN);
    $log->write($tll, "trace: ");
    my $tc     = $_[0];
    $log->write($dll, ': $tc', $tc);
    my $tclass = ref $tc;
    my $key    = $_[1];
    $log->write($dll, ' $key: ',  $key);
    return undef unless $key;
    my $objclass = $tc->{CLASS};
    my $wclass = $tc->{SETTING}{WIDGETCLASS};
    $log->write($nll, '$wclass: ', $wclass);
    my $widget   = $tc->_get_widget($wclass);
    my $storage = $tc->{STORAGE};
    my $setting = $tc->{SETTING};
    my $validate = $setting->{AUTO_VALIDATE};
    $log->write($dll, "\$tc = ", $tc );
    $log->write($dll, 'validate: ', $validate);
    my $value = $_[2];
    $log->write($dll, ' $value: ',  $value);
    my %args = @_[3..$#_];
    $log->write($sll, '%args: ', \%args);
    my $expand   = \%{"$objclass\::tied::EXPAND"};
    my $define   = \%{"$objclass\::tied::DEFINE"};
    my ( $PERSIST, $DEFINE, $EXPAND, $RESPONSE, $ACCEPT, $ERROR, $INVALID )
      = @$storage{@STORAGE};
    my $indexD;
    my $indexE;
    if ( exists $expand->{$key} ) {
      my $inst = $expand->{$key};
      my $super = $inst->[4];
      $log->write($dll, '$super: ', $super);
      my $w = $tc->_get_widget($inst->[0]);
      $log->write($dll, '$w: ', $w);
      $indexE = $expand->{"-$key"};
      $log->write($dll, ' $key: ',  $key);
      $log->write($dll, ' $indexE: ',  $indexE);
      $inst = $expand->{$key};
      $log->write($dll, ' $inst: ',  $inst);
      $EXPAND->[$indexE] = $value;
      if ( exists $define->{$key} ) {
	my $inst = $define->{$key};
	$indexD = $define->{"-$key"};
	$log->write($dll, ' $indexD: ',  $indexD);
	$inst = $define->{$key};
	$log->write($dll, ' $inst: ',  $inst);
	$DEFINE->[$indexD] = $value;
      }
      $log->write($dll, 'BEFORE SUB-COMP PORTION $UPORDOWN: ', $UPORDOWN);
      if( $UPORDOWN ne 'up' and not $w->{ATOMIC} ) {
	my $subs =  $inst->[5];
	$log->write($dll, '$subs: ', $subs);
	# this means it may have sub-components whose value may need
	# to be changed when the super-value (this) valie is changed
	# (using $w->parse(-value => $value )) but we need to keep
	# track of which direction (up or down) the values are
	# propagating
	if ( my $code = UNIVERSAL::can( $widget , 'parse' ) ) {
	  my %l = $w->parse( -value => $value );
	  local $UPORDOWN = 'down';
	  while ( my ($a, $v ) = each %l ) {
	    $log->write($dll, 'FINISHED SUB-COMP PORTION $UPORDOWN: ', $UPORDOWN);
	    $tc->STORE( "$key$a" => $v );
	  }
	}
	$log->write($dll, 'FINISHED SUB-COMP PORTION $UPORDOWN: ', $UPORDOWN);
      }
      $log->write($dll, 'BEFORE SUPER-COMP PORTION $UPORDOWN: ', $UPORDOWN , " => ", $super);
      # need to include `TOP' as a magic action? or as a special accessor?
      if( $UPORDOWN ne 'down' and $super ne 'TOP' ) {
	# this means it has a super-component which may need to be changed when the sub-value (this) is changed (unparse)
	$log->write($dll, '$super: ', $super);
	my $superinst   = $expand->{$super};
	$log->write($dll, '$superinst: ', $superinst);
	my $superclass  = $superinst->[0];
	$log->write($nll, '$superclass: ', $superclass);
	my $superargs   = $superinst->[1];
	$log->write($nll, '$superargs: ', $superargs);
	my $supersuper  = $superinst->[4];
	$log->write($nll, '$supersuper: ', $supersuper);
	my $supersubs   = $superinst->[5];
	$log->write($nll, '$supersubs: ', $supersubs);
	my $superwidget = $tc->_get_widget( $superclass );
	$log->write($nll, '$superwidget: ', $superwidget);
	my $superdefine = $superwidget->{DEFINE};
	$log->write($dll, '$superdefine: ', $superdefine);
	my %pieces;
	foreach my $dk ( keys %$superdefine ) {
	  $log->write($dll, '$dk: ', $dk);
	  my $fk = "$super$dk";
	  $pieces{$dk} = $tc->FETCH( $fk );
	}
	$log->write($nll, '%pieces: ', \%pieces);
	my $unparse = $superwidget->unparse( -name => $super, -pieces => \%pieces );
	$log->write($nll, '$unparse: ', $unparse);
	local $UPORDOWN = 'up';
	$log->write($nll, '$UPORDOWN: ', $UPORDOWN);
	$ERROR->[$indexE]   = ( $validate->{$key}             ) ? $tc->_validate( $key => $value ) : undef;
	$tc->STORE( $super => $unparse );
	$log->write($dll, 'FINISHED SUPER-COMP PORTION $UPORDOWN: ', $UPORDOWN);
      } else {
	$ERROR->[$indexE]   = ( $validate->{$key}             ) ? $tc->_validate( $key => $value ) : undef;
      }
      # I think I'm trying to make INVALID be for top level define stuff and ERROR for expanded stuff
      $INVALID->[$indexD] = ( $indexD and $ERROR->[$indexE] ) ? $ERROR->[$indexE]                : undef;
    } else {
      if ( my $sub = $MAGIC_ACTIONS{$key} || ${"$tclass\::MAGIC_ACTIONS"}{$key}) {
	# for this eventuality, this needs to be inverted so that FETCH does all the work, then STORE will work too because Perl always calls FETCH after STORE when accessed via the tied hash interface
	@MAGICSTORE = ( $value, @_[3..$#_] );
	if ( 0 ) {
	  my @return = $tc->FETCH( $key => $value );
	  $log->write($dll, ' @return: ',  \@return);
	  $log->write($dll, "RETURNING : $key : ", @return > 1 ? @return : $return[0]);
	  return  @return > 1 ? @return : $return[0];
	}
	return 1;
      } elsif ( exists $SETABLE{$key} ) {
	$value = $setting->{$key} = $tc->$sub( $value, @_ );
      } else {
	# we're not allowed to store this here
	$log->write(ERROR, "cannot process key `$key' for `$objclass': does not exist in definition or expand definition and is not a proper action EXPAND: ", join(', ', keys %$expand ), " ::::::  ACTIONS: ", join(', ', keys %MAGIC_ACTIONS, keys %{"$tclass\::MAGIC_ACTIONS"}));
	$value = undef;
      }
    }
    return $value;
  }
}

my %loopy;
my $loopmax = 50;
sub FETCH {
  my $tc  = $_[0];#shift;
  my $tclass  = ref $tc;
  #$log->write($wll, "trace: \$loopy{$tc}=$loopy{$tc}");
  #die "$tc" if $loopy{$tc}++ > $loopmax;
  my $key = $_[1];
  $log->write($dll, ' $key: ',  $key);
  return undef unless $key;
  exists $MAGICSTORE[0] and $_[2] = exists $_[2] ? $_[2] : $MAGICSTORE[0]; # dubious!
  my $value = $_[2];
  $log->write($dll, ' $value: ',  $value);
  my @args = @_[1..$#_];
  my $objclass = $tc->{CLASS};
  $log->write($dll, '$objclass: ', $objclass);
  my $storage = $tc->{STORAGE};
  my $setting = $tc->{SETTING};
  #$log->write($sll, "\$tc = ", $tc );
  my $expand = \%{"$objclass\::tied::EXPAND"};
  #$log->write($dll, ' $expand: ',  $expand);
  my $define = \%{"$objclass\::tied::DEFINE"};
  #$log->write($dll, '$define : ', $define );
  my ( $PERSIST, $DEFINE, $EXPAND, $RESPONSE, $ACCEPT, $ERROR, $INVALID )
    = @$storage{@STORAGE};
  
  my $index;
  my $widget;
  if ( exists $expand->{$key} ) {
    $index = $expand->{"-$key"};
    $value = $EXPAND->[$index];
  } else {
    $log->write($ill, "ABOUT TO CALL MAGIC ACTION: for \$key=$key: ", $objclass);
      if ( my $sub = $MAGIC_ACTIONS{$key} || ${"$tclass\::MAGIC_ACTIONS"}{$key}) {
      $log->write($ill,'$sub : ', $sub , " ($objclass)");
      $log->write($dll, "CALLING MAGIC ACTION: for \$key=$key: ", $sub, \@args);
      if ( UNIVERSAL::can( $tc, $sub ) ) {
	$value = $tc->$sub( @args );
      } else {
	$log->write($ill, "CALLING MAGIC ACTION FAILED: for \$key=$key: ", $sub, \@args, \%MAGIC_ACTIONS, \%{"$tclass\::MAGIC_ACTIONS"});
      }
      @MAGICSTORE = ();
      $log->write($dll, "RETURNING : \$tc->$sub( \@args ) = ", $value);
    } elsif ( exists $SETABLE{$key} ) {
      $value = $setting->{$key};
      $log->write($dll, "RETURNING : \$setting->{$key} : ", $value);
    } else {
      # we're not allowed to store this here
      $log->write(CRIT, "cannot process key `$key' for `$objclass': does not exist in definition and is not a proper action DEFINE: ", join(', ', keys %$define ), " ::::::  ACTIONS: ", join(', ', keys %MAGIC_ACTIONS, keys %{"$tclass\::MAGIC_ACTIONS"}));
    }
  }
  return $value;
}

sub FETCHSIZE {
  $log->write($tll, "trace: ");
  my $tc = shift;
  my $objclass = $tc->{CLASS};
  my $storage = $tc->{STORAGE};
  my $setting = $tc->{SETTING};
  #$log->write($sll, "\$tc = ", $tc );
  my $key   = shift;
  my $define   = \%{"$objclass\::tied::DEFINE"};
  #$log->write($sll, "define: ", $define);
  $log->write($dll, "length define: ", length %$define);
  my $td = tied %$define;
  #$log->write($sll, "td: ", $td);
  my $size = $td->{STATS}{SIZE};
  $log->write($dll, "td: SIZE", $size);
  return $size;
}
{
  sub FIRSTKEY {
    $log->write($tll, ':',);
    my $tc = shift;
    my $objclass = $tc->{CLASS};
    my $define;
    if ( $tc->{IAMEXPANDED} ) { # =~/^EXPANDED\:/ ) {
      $define   = \%{"$objclass\::tied::EXPAND"};
    } else {
      $define   = \%{"$objclass\::tied::DEFINE"};
    }
    return each %$define;
  }
  sub NEXTKEY {
    $log->write($tll, ':',);
    my $tc = shift;
    my $objclass = $tc->{CLASS};
    my $define;
    if ( $tc->{IAMEXPANDED} ) { # =~/^EXPANDED\:/ ) {
      $define   = \%{"$objclass\::tied::EXPAND"};
    } else {
      $define   = \%{"$objclass\::tied::DEFINE"};
    }
    return each %$define;
  }
}

sub DELETE {
  $log->write($tll, "trace: ");
  my $tc = shift;
  my $objclass = $tc->{CLASS};
  my $key = shift;
  my $define   = \%{"$objclass\::tied::DEFINE"};
  my $td = tied %$define;
  my $expand = \%{"$objclass\::tied::EXPAND"};
  my $tx = tied %$expand;
  my $exp = $tx->{$key}[4];
  $log->write($dll, ' $exp: ',  $exp);
  foreach ( @$exp ) {
    $tx->DELETE( $_ );
  }
  $td->DELETE( $key );
}

sub EXISTS {
  $log->write($tll, "trace: ");
  my $tc = shift;
  my $objclass = $tc->{CLASS};
  my $define   = \%{"$objclass\::tied::DEFINE"};
  my $td = tied %$define;
  my $expand = \%{"$objclass\::tied::EXPAND"};
  my $tx = tied %$expand;
  $td->EXISTS( @_ ) || $tx->EXISTS( @_ );
}

sub CLEAR     {
  $log->write($tll, "trace: ");
  my $tc = shift;
  my $storage = $tc->{STORAGE};
  $log->write($tll, "TRACING: ", \@_ );
  @$storage{@STORAGE} = map { [] } @STORAGE;
  return;
}

# SETTINGS
sub _set_auto_validation {
  $log->write($tll, "trace: ");
  my $tc   = shift;
  my $objclass = $tc->{CLASS};
  my $pieces = shift;
  my $define   = \%{"$objclass\::tied::DEFINE"};
  my $td = tied %$define;
  my $expand = \%{"$objclass\::tied::EXPAND"};
  my $tx = tied %$expand;
  my $validate = {};
  my $ref = ref $pieces;
  if ( $pieces and not $ref ) { #validate everything
    foreach my $key ( keys %$define ) {
      $validate->{$key} = 1;
    }
    foreach my $key ( keys %$expand ) {
      $validate->{$key} = 1;
    }
  } elsif( $ref ) { # validate only the entries in the pieces list
    $ref eq 'ARRAY' and $pieces = { map { $_ => $_ } @$pieces };
    while ( my ( $d, $v ) = each %$pieces ) {
      $validate->{$d} = $v;
    }
  }
  $log->write($dll, '$validate: ', $validate);
  
  return $validate;
}

sub _set_immutable       {}
sub _set_init            { return $_[1] }
sub _set_widgetclass     { return $_[1] }

sub _initialize {
  my $tc = shift;
  $log->write($dll, '$tc: ', $tc);
  _expand( $tc );
  _expanded( $tc );
  #_validate( $tc );
}

{
  my $max_deep = 100;# levels of recursion allowed before we barf
  my $max_attr = 100;# number of immediate sub attributes allowed before we complain
  my $count = 0;

  sub _expander {
    $log->write($dll, ':',);
    $count = 0;
    my $tc = shift;
    my $objclass = $tc->{CLASS};
    my %lineage;
    tie %lineage, _ARRAYCLASS_;
    my $expand = shift or $log->write(CRIT, 'usage _expand( $tc, $expand)');
    $log->write($dll, ' $expand: ',  $expand);
    my $wclass = $tc->{SETTING}{WIDGETCLASS};
    my $strands = \%{"$wclass\::tied::STRANDS"};
    _expand__( $tc, -lineage => \%lineage, -expand => $expand, -args => {}, -strands => $strands );
    $log->write($dll, ' $expand: ',  $expand);
    return $expand;
  }

  sub _expand__ {
    $log->write($tll, "trace:");
    $count++;
    $log->write(CRIT, "Exceeded maximal recursion count: $max_deep : ", $count) if $count >= $max_deep;
    my $tc       = shift;
    my %args = @_;
    my $args        = $args{-args};
    my $persistargs = $args{-persistargs};
    my $lineage  = $args{-lineage};
    my $prefix   = $args{-prefix} || '';
    my $expand   = $args{-expand};
    my $strands  = $args{-strands};
    $log->write($dll, ' $prefix: ',  $prefix);
    my $class    = ref $tc || $tc;
    my $objclass = $tc->{CLASS};
    my $strand   = $tc->{STRAND};
    my $wclass    = $tc->{SETTING}{WIDGETCLASS};
    $log->write($dll, '$wclass: ', $wclass);
    my $define   = \%{"$objclass\::tied::DEFINE"};
    my $atomic = _atomic( $tc );
    $log->write($dll, " _atomic: `$wclass'", $atomic);
    if ( $atomic ) {
      my $key = $define->{'-0'};
      my $val = $define->{'0'};
      my $sargs = $val->[1];
      my $pargs = $val->[3];
      $log->write($dll, 'about to merge hash args: ', [ $args, $sargs ]);
      my $myargs = _merge_hash( $args, $sargs );
      my $mypersistargs = _merge_hash( $persistargs, $pargs );
      $log->write($dll, '$myargs: ', $myargs);
      $log->write($dll, 'About to insert $key: ', $key);
      $expand->{$key}  = [ $wclass => $myargs ];
      $log->write($dll, "\$expand->{$key}: ", $expand->{$key});
      $strands->{$strand} = $wclass;
    } else {
      while ( my ($k => $v) = each %$define ) {
	my $key = "$prefix$k";
	$log->write($dll, '$key: ', $key);
	my $name = "$wclass#$key";
	$log->write($nll, ' $name: ',  $name, ', $k: ', $k, ', $v: ', $v, ', $key: ', $key);
	my $wc = _strand_to_widgetclass( $v->[0] );
	$log->write($dll, ' $wc: ',  $wc);
	my $w = $tc->_get_widget( $wc );
	$log->write($ill, '$w: ', $w);
	my $c = $w->strand();
	$strands->{$c} ||= $wc;
	my $d = $w->{DEFINE};
	$log->write($dll, '$d: ', $d);
        $log->write($dll, '$w->{ATOMIC}: ', $w->{ATOMIC});
	my $map = [ map { "$key$_";} ( $w->{ATOMIC} ? ("") : (keys %{$d})) ];
	$log->write($dll, ' $map: for ', $wc, " => ",  $map);
	my $sargs = $args->{$k} || {};
	my $pargs = $persistargs->{$k} || {};
	$log->write($dll, 'about to merge args: ', [ ref $w, $sargs, $v->[1] ]);
	# myargs are for the method calls to this widget
	# subargs are for additional processing/merging destined for the sub-component widget method calls
	my ( $myargs       , $subargs        ) = _merge_args( $w, $sargs, $v->[1] );
	my ( $mypersistargs, $subpersistargs ) = _merge_args( $w, $pargs, $v->[3] );
	$log->write($dll, '$myargs: ', $myargs);
	$log->write($dll, '$subargs: ', $subargs);
	my $persist     = '';
	$log->write($dll, 'About to insert $key: ', $key);
	$expand->{$key}  = [ $wc               => $myargs       , # 0 => 1, ==> widget_class               => arguments bound for the $wc widget method calls
			     $persist          => $mypersistargs, # 2 => 3, ==> (reserved for stowage use) => (reserved for stowage use)
			     ($prefix||'TOP')  => $map          , # 4 => 5, ==> <PARENT_ATTR_NAME>         => [list of sub-component attribute names]
			   ];
	unless ( $w->{ATOMIC} ) {
	  $log->write($dll, 'About to recursively call _expand__ for: ', ref tied %$w, " ($wc)");
	  _expand__( tied %$w, -lineage => $lineage, -prefix => $key, -expand => $expand, -args => $subargs, -persistargs => $subpersistargs );
	}
      }
    }
    $log->write($dll, '$expand: ',  $expand);
    return $expand;
  }

  sub _merge_args {
    # this deals with passing arguments from one widget definition to
    # the method calls of sub-widgets, actual argument contents are
    # evaluated at execution time withing the action being executed,
    # therefore each method must deal with things like CODE refs and
    # such as is applicable locally

    # this merges a set of arguments given as hash references, earlier
    # hashes in the list take precedence over later ones. This is be
    # somewhat recursive , via _expand__ because of $subargs. Since we
    # are determining argument precedence here, we may want to take
    # into account the contents of the -all argument and make it copy
    # into the rest of the more specific argument designations (like
    # -validate, or -display). Alternately, we may want to leave that
    # for runtume method processing, but I can see potential proplems
    # down that road. Obviously, the more specific arguments take
    # precedence over the -all argument. I'm not sure if an argument
    # such as -required should propagate to the end of the widget
    # branch (or if any arguments other than specifically designated
    # ones should propagate at all beyond the current widget)

    my $widget = shift;
    my $wclass = _strand_to_widgetclass( $widget->strand() );
    $log->write($dll, ' $wclass: ',  $wclass);
    my $define = $widget->{DEFINE};
    my (%myargs, %subargs);
    # for each hash passed in process it as an argument list, with the earlier items overriding the later items
    foreach my $h ( reverse @_ ) {
      while ( my ($k, $v) = each %$h ) {
        $log->write($dll, "WIDGET: $wclass => ", '$k: ', $k);
	$log->write($dll, "WIDGET: $wclass => ", '$v: ', $v);
	if ( $k =~ /^\-/ ) {
	  # begins with a minus sign, is an argument for THIS widget
	  $log->write($ill, "WIDGET: $wclass => `$k' ... ", 'begins with a minus sign, is an argument for THIS widget:',);
	  if ( exists $myargs{$k} ) {
	    if ( 0 ) {
	      $myargs{$k} = [ $myargs{$k} ] unless ref $myargs{$k} eq 'ARRAY';
	      push @{$myargs{$k}}, $v;
	      $log->write($dll, "WIDGET: $wclass => `$k' ... ", "pushed another arg list onto \$myargs{$k} length=", scalar @{$myargs{$k}});
	    }else{
	      if ( ref $myargs{$k} eq 'HASH' ) {
		$log->write($dll, "WIDGET: $wclass => `$k' ... ", '$myargs{$k} exists we may need to merge existing args with new args:',);
		if ( ref $v eq 'HASH' ) {
		  $log->write($dll, "WIDGET: $wclass => `$k' ... ", "ref \$myargs{$k} eq 'HASH' need to merge existing args with new args: calling _merge_hash(\$v, \$myargs{$k})",);
		  $myargs{$k} = _merge_hash($v,$myargs{$k});
		} else {
		  $log->write($dll, "WIDGET: $wclass => `$k' ... ", "ref \$myargs{$k} ne 'HASH'  new args override old args:");
		  $myargs{$k} = $v;
		}
	      }
	    }
	  }else{
	    $log->write($dll, "WIDGET: $wclass => ", "first placement of \$myargs{$k}:");
	    $myargs{$k} = $v;
	  }
	}elsif( exists $$define{$k} ) {
	  $log->write($ill, "WIDGET: $wclass => `", $k, '\' is the name of a sub-component, arguments to be passed to sub-component: ');
	  if ( exists $myargs{$k} ) {
	    if ( 0 ) {
	      $subargs{$k} = [ $subargs{$k} ] unless ref $subargs{$k} eq 'ARRAY';
	      push @{$subargs{$k}}, $v;
	      $log->write($dll, "WIDGET: $wclass => ", "pushed another arg list onto \$subargs{$k} length=", scalar @{$subargs{$k}}, $subargs{$k});
	    } else {
	      if ( ref $subargs{$k} eq 'HASH' ) {
		$log->write($dll, "WIDGET: $wclass => ", '$subargs{$k} exists we may need to merge existing args with new args:',);
		if ( ref $v eq 'HASH' ) {
		  $log->write($dll, "WIDGET: $wclass => ", "ref \$subargs{$k} eq 'HASH' need to merge existing args with new args: calling _merge_hash(\$v, \$subargs{$k})",);
		  # this is bogus because _merger_args returns a two element list, need to rethink
		  $subargs{$k} = _merge_hash($v,$subargs{$k});
		} else {
		  $log->write($dll, "WIDGET: $wclass => ", "ref \$subargs{$k} ne 'HASH'  new args override old args:");
		  $subargs{$k} = $v;
		}
	      }
	    }
	  }else{
	    $log->write($dll, "WIDGET: $wclass => ", "first placement of \$subargs{$k}:");
	    $subargs{$k} = $v;
	  }
	}else{
	  # hmmm, for where else can it be destined?
	  $log->write($ill, "WIDGET: $wclass => ", "# hmmm, for where else can it be destined?: $k => ", $v);
	}
      }
    }
    return (\%myargs, \%subargs);
  }
  
  sub _merge_hash {
    my %hash = ();
    foreach my $h ( reverse @_ ) {
      while ( my ($k, $v) = each %$h ) {
        $log->write($dll, '$k: ', $k);
	$log->write($dll, '$v: ', $v);
	if ( exists $hash{$k} ) {
	  if ( ref $hash{$k} eq 'HASH' ) {
	    $log->write($dll, '$hash{$k} exists we may need to merge existing args with new args:',);
	    if ( ref $v eq 'HASH' ) {
	      $log->write($dll, "ref \$hash{$k} eq 'HASH' and ref \$v eq 'HASH' need to merge existing args with new args: calling _merge_hash(\$v, \$hash{$k})",);
	      $hash{$k} = _merge_hash($v,$hash{$k});
	    } else {
	      $log->write($dll, "ref \$v ne 'HASH'  new args override old args:");
	      $hash{$k} = $v;
	    }
	  } else {
	    $log->write($dll, "ref \$hash{$k} ne 'HASH'  new args override old args:");
	    $hash{$k} = $v;
	  }
	} else {
	  $log->write($dll, "first placement of \$hash{$k}:");
	  $hash{$k} = $v;
	}
      }
    }
    return \%hash;
  }
  

  sub _strand_to_widgetclass {
    my $strand = shift;
    my $wclass  =  $strand =~ /\:\:/ ? $strand  # fully qualified widget
      : "Object::Strand::$strand";      # standard widget class
    $log->write($dll, '$wclass: ', $wclass);
    $log->write($dll, '$strand: ', $strand);
    return $wclass;
  }

}

sub _merge_hashes {
  my $a = shift;
  my $b = shift;
  #$log->write($sll, "a: ", $a);
  #$log->write($sll, "b: ", $b);
  my @return = map {
    my $p = { (exists $a->{$_} ? (ref $a->{$_} eq 'HASH' ? ( %{$a->{$_}} ) : ( $_ => $a->{$_})):()) };
    my $q = { (exists $b->{$_} ? (ref $b->{$_} eq 'HASH' ? ( %{$b->{$_}} ) : ( $_ => $b->{$_})):()) };
    #$log->write($sll, "p: ", $p);
    #$log->write($sll, "q: ", $q);
    #$log->write($sll, "\%q, \%p: ", { %$q, %$p } );
    ($_ =>{ %$q, %$p });
  } ((keys %$a), (keys %$b));
  #$log->write($sll, "\@return:", \@return);
  return @return;
}
####################
# MAGIC_ACTIONS

sub _define      {
  $log->write($tll, "trace: ");
  my $tc     = shift;
  my $objclass = $tc->{CLASS};
  #$log->write($sll, "th ", $tc);
  my $define   = \%{"$objclass\::tied::DEFINE"};
  return $define;
}

sub _strands      {
  $log->write($tll, "trace: ");
  my $tc     = shift;
  my $objclass = $tc->{CLASS};
  #$log->write($sll, "th ", $tc);
  my $strands   = \%{"$objclass\::tied::STRANDS"};
  $log->write($dll, '$strands: ', $strands);
  return $strands;
}

sub _dednapxe      {
  return 'THIS SHOULD RETURN FROM THE FULL EXPAND VIEW ONLY THE ELEMENTS THAT ARE ATOMIC';
}

sub _expand      {
  $log->write($tll, "trace: ");
  my $tc     = shift;
  my %args = @_;
  my $objclass = $tc->{CLASS};
  #$log->write($sll, "th ", $tc);
  my $expand = \%{"$objclass\::tied::EXPAND"};
  #$log->write($sll, '$expand: ', $expand);
  tie %$expand, _ARRAYCLASS_ unless tied %$expand;
  if ( scalar keys %$expand ) {
    $log->write($dll, 'already _expand\'ed: ', $objclass, " ... ", $expand);
  } else {
    $log->write($dll, 'about to _expand: ', $objclass, " ... ", $expand);
    _expander( $tc, $expand );
    $log->write($dll, 'finished _expand\'ing: ', $objclass, " ::: " , \%{"$objclass\::tied::EXPAND"} );
  }
  $log->write($dll, 'RETURNING $expand: ',  $expand);
  return $expand;
}

sub _atomics      {
  $log->write($tll, "trace: ");
  my $tc     = shift;
  my $wclass = $tc->{SETTING}{WIDGETCLASS};
  #$log->write($sll, "th ", $tc);
  my $atomics = \@{"$wclass\::tied::ATOMICS"};
  #$log->write($sll, '$atomics: ', $atomics);
  if ( @$atomics ) {
    $log->write($dll, 'already _atomics\'ed: ', $wclass, " ... ", $atomics);
  } else {
    my $expand = _expand($tc);
    foreach my $k ( keys %$expand ) {
      my $w = $tc->_get_widget( $expand->{$k}[0] );
      push @$atomics, $k if $w->{ATOMIC};
    }
  }
  return $atomics;
}


sub _expanded  {
  $log->write($tll, "trace: ");
  my $tc     = shift;
  my $objclass = $tc->{CLASS};
  my $expanded = $tc->{EXPANDED};
  unless( $expanded ) {
    #$log->write($sll, "th ", $tc);
    my $expand = \%{"$objclass\::tied::EXPAND"} || $tc->_expand( @_ );
    my %expanded = ();
    my @args = ( SETTING => $tc->{SETTING}, EXPAND => $expand, DEFINE => $expand, CLASS => $objclass );
    @args = ( CLASS => $objclass );
    #$log->write($dll, "args to tie: ", \@args);
    my $ts = tie %expanded, __PACKAGE__, @args;
    $expanded = bless \%expanded, $objclass;
    $tc->{EXPANDED} = $expanded;
    $ts->{STORAGE}  = $tc->{STORAGE};
    $ts->{SETTING} = $tc->{SETTING};
    $ts->{IAMEXPANDED} = 'EXPANDED:' . $expanded;
  }
  $log->write($dll, "RETURNING \$expanded: $expanded");
  return $expanded;
}

sub _verify       {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD PREPARE A SPECIAL FORM FOR THE USER TO VERIFY THAT PREVIOUSLY ENTERED DATA WAS CORRECT';
}

sub _accept       {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD MOVE THE RESPONSE DATA TO THE VALUE DATA, QUEUEING IT UP FOR A COMMIT';
}

sub _display     {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD DISPLAY A REPRESENTATION OF THE DATA IN PERMANENT STORAGE';
}

sub _persist     {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD RETURN A COPY OF THE DATA IN PERMANENT STORAGE';
}

sub _value       {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD RETURN THE CURRENTLY ACCEPTED VALUES (QUEUED HERE FOR A COMMIT)';
}

sub _rollback       {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD ABORT THE CURRENTLY ACCEPTED VALUES (DISCARD QUEUE)';
}

sub _commit       {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD EXECUTE THE CURRENTLY ACCEPTED VALUES (COMMIT QUEUED DATA TO PERMANENT STORAGE)';
}

sub _response    {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD RETURN THE VALUES ENTERED BY THE USER WHEN PROMPTED FOR DATA (RAW DATA)';
}

sub _store    {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD RETURN THE RESPONSE VALUES WITH APPROPRIATE SUB-STRAND DATA MUNGING (PRETTY DATA)';
}

sub _error       {
  $log->write($tll, "trace: ");
  my $tc  = shift;
  $log->write($dll, '$tc: ', $tc);
  my $key = shift;
  $log->write($dll, '$key: ', $key);
  my $value = $MAGICSTORE[0];
  $log->write($dll, '$value: ', $value);
  
  my $storage = $tc->{STORAGE};
  my $errors  = $storage->{ERROR};
  $log->write($dll, ' $errors: ',  $errors);
  #return { map { $_ ? %$_ : ()} @$errors };
  my $return = $errors;
  if ( $value ) {
    my $expand = _expand( $tc );
    #$log->write($dll, '$expand: ', $expand);
    $log->write($dll, 'keys %$expand: ', [ keys %$expand ]);
    my $index = $expand->{"-$value"};
    $log->write($dll, '$index: ', $index);
    $return = $return->[$index] ? $return->[$index] : undef;
    $log->write($dll, '$return: ', $return);
  }
  $log->write($dll, '$return: ', $return);
  return $return ? $return : ();
}

sub _invalid       {
  $log->write($tll, "trace: ");
  my $tc     = shift;
  my $expand = _expand( $tc );
  my $storage = $tc->{STORAGE};
  my $invalid  = $storage->{ERROR};
  my @invalid;
  my $index;
  foreach my $k ( keys %$expand ) {
    $index = $expand->{"-$k"};
    push @invalid, $k => $invalid->[$index] if ( defined $invalid->[$index] );
  }
  return scalar @invalid ? \@invalid : ();

}

sub _option_list       {
  $log->write($tll, "trace: ");
  my $tc     = shift;
  $log->write($dll, '$tc: ', $tc);
  my $expand = _expand( $tc );
  my $settings = $tc->{SETTING};
  my $key = shift;
  $log->write($dll, '$settings: ', $settings);
  my $list = $settings->{$key};
  $log->write($dll, '$list: ', $list);
  $log->write($dll, '@MAGICSTORE: ', \@MAGICSTORE);
  if ( my $value  = exists $_[0] ? (shift) : $MAGICSTORE[0] ) {
    $log->write($dll, '$value: ', $value);
    $list = $settings->{OPTION_LIST} = $value;
  }
  $log->write($dll, '$list: ', $list);
  return $list;
}

sub _input       {
  $log->write($tll, "trace: ");
  $log->write($dll, "trace: ");
  my $tc     = shift;
  #$log->write($sll, ' $tc: ',  $tc);
  my $objclass = $tc->{CLASS};
  $log->write($dll, '$objclass: ', $objclass);
  my $wclass = $tc->{SETTING}{WIDGETCLASS};
  $log->write($dll, '$wclass: ', $wclass);
  my $widget = $tc->_get_widget( $wclass );
  $log->write($dll, '$widget: ', $widget);
  my $key    = shift;
  $log->write($dll, ' $key: ',  $key);
  my $value  = shift;
  $log->write($dll, ' $value: ',  $value);
  my %args = @_;
  my ( @k, @v, %kv, %ki );
  my $ki = $args{-input} || \%ki;
  my $storage = $tc->{STORAGE};
  $log->write($dll, ' $storage: ',  $storage);
  my ( $PERSIST, $DEFINE, $EXPAND, $RESPONSE, $ACCEPT, $ERROR, $INVALID )
    = @$storage{@STORAGE};
  my $setting = $tc->{SETTING};
  $log->write($dll, ' $setting: ',  $setting);
  my $expand = $widget->{EXPAND};
  $log->write($dll, '$expand: ', $expand);
  my $define   = $widget->{DEFINE};
  $log->write($dll, '$define: ', $define);
  if ( exists $expand->{$key} ) {
    # we are inputing a single element
    @k = ( $key   );
    @v = ( $value );
    @kv{@k} = @v;
  } elsif ( $key eq 'INPUT' ) {
    # we are inputing everything
    @k = ( keys %$define );
    @v = ();
  }
  my @input;
  $log->write($dll, '@k: ', \@k);
  foreach my $k ( @k ) {
    $log->write($dll, '$k: ', $k);
    $kv{$k} = exists $kv{$k} ? $kv{$k} : $tc->FETCH( $k );
    my $index  = $expand->{"-$k"};
    my $inst   = $expand->{$k};
    $log->write($dll, '$inst: ', $inst);
    my $wc = $inst->[0];
    my $w = $tc->_get_widget( $wc );
    my @i;
    unless( $w->{ATOMIC} ) {
      foreach my $attr ( @{$inst->[5]} ) {
	$kv{$attr} = exists $kv{$attr} ? $kv{$attr} : $tc->FETCH( $attr );
	$tc->_input( $attr => $kv{$attr}, -input => $ki );
	push @i, $attr if $ki->{$attr};
      }
    }
    my %i = {};
    my $i;
    if ( @i ) {
      $log->write($dll, 'There was input from subcomponents:', \@i);
      @i{@i} = @$ki{@i};
      $log->write($dll, 'There was input from subcomponents:', \%i);
    } else {
      $log->write($dll, 'There was NOT input from subcomponents:', \@i);
      my $can = UNIVERSAL::can( $wc, 'input');
      $log->write($dll, ' $can: ', $wc, " : input? ", $can);
      if( $can ) {
	my %input_args = ( -name => $k, -value => $kv{$k}, %{$inst->[1]{-all}||{}}, %{$inst->[1]{-input}||{}});
	$log->write($dll, '%input_args: for ', $wc , " => ", \%input_args);
	$i = $w->input( %input_args );
	$log->write($dll, ' $i: ',  $i);
	my $ind;
	while ( my ( $sk, $sv ) = each %$i ) {
	  $ki{$sk} = $sv;
	  $ind = $expand->{"-$sk"};
	  $RESPONSE->[$ind] = $sv;
	}
      }
    }
    push @input, $k if ( @i or $ki->{$k} );
  }
  $log->write($dll, "INPUT: ", \@input);
  $log->write($dll, '$expand: ', $expand);
  return $ki;
  #return 'THIS SHOULD PREPARE A FORM FOR THE USER TO ENTER DATA';
}


sub _validate    {
  $log->write($dll, "trace: ");
  my $tc     = shift;
  my $objclass = $tc->{CLASS};
  my $wclass = $tc->{SETTING}{WIDGETCLASS};
  my $widget = $tc->_get_widget( $wclass );
  $log->write($dll, '$widget: ', $widget);
  $log->write($dll, ' $tc: ',  $tc);
  my $key    = shift;
  $log->write($dll, ' $key: ',  $key);
  my $value  = exists $_[0] ? (shift) : $MAGICSTORE[0];
  $log->write($dll, ' $value: ',  $value);
  my %args = @_;
  my ( @k, @v, %kv, %ke );
  my $ke = $args{-errors} || \%ke;
  my $storage = $tc->{STORAGE};
  $log->write($dll, ' $storage: ',  $storage);
  my ( $PERSIST, $DEFINE, $EXPAND, $RESPONSE, $ACCEPT, $ERROR, $INVALID )
    = @$storage{@STORAGE};
  my $setting = $tc->{SETTING};
  $log->write($dll, ' $setting: ',  $setting);
  my $expand = $widget->{EXPAND};
  $log->write($dll, '$expand: ', $expand);
  my $define   = $widget->{DEFINE};
  $log->write($dll, '$define: ', $define);
  my $single;
  if ( exists $expand->{$key} ) {
    # we are validating a single element
    $single = $key;
    @k = ( $key   );
    @v = ( $value );
    @kv{@k} = @v;
  } elsif ( $key eq 'VALIDATE' ) {
    # we are validating everything
    @k = ( keys %$define );
    @v = ();
  }
  my @error;
  $log->write($dll, '@k: ', \@k);
  foreach my $k ( @k ) {
    $log->write($dll, '$k: ', $k);
    $kv{$k} = exists $kv{$k} ? $kv{$k} : $tc->FETCH( $k );
    my $index  = $expand->{"-$k"};
    $log->write($dll, '$index: ', $index);
    my $inst   = $expand->{$k};
    $log->write($dll, '$inst: ', $inst);
    my $wc = $inst->[0];
    my $w = $tc->_get_widget( $wc );
    my (@e, %e, $e);
    my $can = UNIVERSAL::can( $wc, 'validate');
    $log->write($dll, ' $can: ', $wc, " : validate? ", $can);
    if( $can ) {
      # first we validate the TOP level represented by this key
      my %validate_args = ( -name => $k,
			    -value => $kv{$k},
			    %{$inst->[1]{-all}      || {}},
			    %{$inst->[1]{-validate} || {}},
			  );
      $log->write($dll, '%validate_args: for ', $wc , " => ", \%validate_args);
      $e = $w->validate( %validate_args );
      $log->write($dll, ' $e: ',  $e);
      if ( $e->{ERROR} ) {
	$ERROR->[$index] = $ke->{$k} = $e;
      }
    }
    if ($ke->{$k} ) {
      #$log->write($dll, "TOP level error: $k ::: ", $ke->{$k});
      push @error, $k;
    } #else {
      #$log->write($dll, "No TOP level errors");
      # if there were no errors in the TOP level, then we check for potential errors in the sub components
      unless( $w->{ATOMIC} ) {
	foreach my $attr ( @{$inst->[5]} ) {
	  $kv{$attr} = exists $kv{$attr} ? $kv{$attr} : $tc->FETCH( $attr );
	  $tc->_validate( $attr => $kv{$attr}, -errors => $ke );
	  push @e, $attr if $ke->{$attr};
	}
      }
      if ( @e ) {
	$log->write($dll, 'There were errors in subcomponents:', \@e);
	@e{@e} = @$ke{@e};
	$log->write($dll, 'There were errors in subcomponents:', \%e);
      }
    #}
    push @error, $k if @e;
  }
  $log->write($dll, "VALIDATE: ", \@error);
  $log->write($dll, '$expand: ', $expand);
  if ( join('', @error) ) {
    return $single ? $ke->{$single} : $ke;
  } else {
    return undef;
  }
}

sub _lint        {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD CHECK THE STRAND FOR INTERNAL CONSISTENCY';
}

sub _args        {
  $log->write($tll, "trace: ");
  return 'THIS SHOULD RETURNS THE MERGED TREE OF EXECUTION ARGUMENTS SPECIFIED IN THE @DEFINE FOR EACH SUPER-COMPONENT, SUITABLE FOR SUB-COMPONENT RUNTIME CONSUMPTION';
}

sub _get_widget {
  my $tc = shift;
  my $wc = shift or $log->write(CRIT, 'widget class must be supplied: ');
  UNIVERSAL::isa( $wc, __PACKAGE__) and $wc = shift;
  $wc =~ /\:\:/ or $wc = "Object::Strand::$wc";
  $log->write($nll, '$wc: ', $wc);
  my $w;
  eval { $w = ${"$wc\::WIDGET"} ||= $wc->new() or $log->write(CRIT, 'Couldn\'t get a widget for: ', $wc);};
  $log->write(CRIT, 'eval error: ', $@) if $@;
  $log->write($dll, '$w: ', $w);
  return $w;
}

sub _top {
  my $tc = shift;
  $log->write($nll, '$tc: ', $tc);
  my $objclass = $tc->{CLASS};
  my $define   = \%{"$objclass\::tied::DEFINE"};
  $log->write($dll, ': ', \@_ );
  if ( $tc->{IAMEXPANDED} ) {
    $log->write(CRIT, 'you can not call TOP on an EXPANDED view', );
  }
  my $expanded = $tc->{EXPANDED};
  my %pieces;
  if ( scalar @_ == 2 ) {
    # we are STORE'ing the TOP value, so we cut it up and propagate the pieces to the subcomponents
    %pieces = $expanded->parse( -value => $_[1] );
    $log->write($nll, '%pieces: ', "(from $_[1]) ::: ", \%pieces );
    while ( my ($a, $v ) = each %pieces ) {
      $tc->STORE( $a => $v );
    }
  } else {
    # we are FETCH'ing the TOP value, so we have to paste it together from the sub-component values
    foreach my $k ( keys %$define ) {
      $log->write($dll, '$k: ', $k);
      $pieces{$k} = $tc->FETCH( $k );
    }
  }
  $log->write($nll, '%pieces: ', \%pieces );
  my $unparse = $expanded->unparse( -pieces => \%pieces );
  $log->write($nll, '$unparse: ', $unparse);
  return $unparse;
}



sub _atomic      {
  my $tc = shift;
  my $objclass = $tc->{CLASS};
  #$log->write($nll, '$objclass: ', $objclass);
  my $wclass = $tc->{SETTING}{WIDGETCLASS};
  #$log->write($nll, '$objclass: ', $objclass);
  my $atomic   = \${"$wclass\::tied::ATOMIC"};
  my $checked   = \${"$wclass\::tied::ATOMIC_CHECKED"};
  return $$atomic if ( defined $$atomic and defined $$checked );
  my $ndefine   = \@{"$objclass\::DEFINE"};
  my $hdefine   = \%{"$objclass\::tied::DEFINE"};
  #$log->write($dll, "\nref '\$hdefine->{\$hdefine->{'-0'}} eq 'ARRAY' and \$hdefine->{'-0'} eq \$hdefine->{\$hdefine->{'-0'}}[0] and scalar keys %\$hdefine == 1`", ref $hdefine->{$hdefine->{'-0'}}, "' eq 'ARRAY' and $hdefine->{'-0'} eq $hdefine->{$hdefine->{'-0'}}[0] and ", (scalar keys %$hdefine), " == 1" );
  $log->write($ill, "$objclass: ", scalar @$ndefine);
  $log->write($dll, "$objclass: ", $ndefine);
  if ( $$atomic or not scalar @$ndefine
      #      ref $hdefine->{$hdefine->{'-0'}} eq 'ARRAY'
      #      and $hdefine->{'-0'} eq $hdefine->{$hdefine->{'-0'}}[0]
      #      and scalar keys %$hdefine == 1
     ) {
    #$log->write($nll, "ATOMIC YES: ", $hdefine);
    *{"$objclass\::tied::EXPAND"} = $hdefine;
    $hdefine->{$tc->{STRAND}}[0] = $tc->{SETTING}{WIDGETCLASS};
    $hdefine->{$tc->{STRAND}}[4] = 'TOP';
    $hdefine->{$tc->{STRAND}}[5] = [ $tc->{STRAND} ];
    return $$atomic = 1;
  } else {
    #$log->write($nll, "ATOMIC NO: ", $hdefine);
    return $$atomic = 0;
  }
}
#
####################

1;
__END__

__END__
package Tie::Strand;

use 5.006;
use strict;
use warnings;

require Exporter;
use AutoLoader qw(AUTOLOAD);
use Taowebs::Log qw(:all);

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(  ) ] );
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw(  );
our $VERSION = '0.01';

# Preloaded methods go here.

# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__
=head1 NAME

Tie::Strand - <BRIEF DESC>

=head1 SYNOPSIS

  use Tie::Strand;
  <BRIEF USAGE>

=head1 DESCRIPTION

<LONG DESC>


=head2 EXPORT

None by default.


=head1 HISTORY

=over 8

=item 0.01

Original version; created by h2xs 1.1.1.1 with options

  -CXn
	Tie::Strand

=back


=head1 AUTHOR

Theo Lengyel, E<lt>theo@taowebs.net<gt>

=head1 SEE ALSO

L<perl>.

=cut
