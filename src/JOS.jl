mutable struct Instance
  class
  applicable
  func
  slots
end
(inst::Instance)(args...) = getfield(inst, :func)(args...)

#We now make a radical move, that forbids the use of . for the low-level access to the instance representations
Base.getproperty(inst::Instance, sym::Symbol) = slot_ref(inst, sym)
Base.setproperty!(inst::Instance, sym::Symbol, val) = slot_set!(inst, sym, val)

_instance_ref(o, f) = getfield(o, :slots)[f]
_instance_set!(o, f, n) = getfield(o, :slots)[f] = n
is_instance(o) = o isa Instance
instance_class(o) = getfield(o, :class)
instance_slots(o) = getfield(o, :slots)
set_instance_class_to_self!(c) = setfield!(c, :class, c)
set_instance_proc!(o, f) = 
  getfield(o, :applicable) ? 
    setfield!(o, :func, f) :
    error("Can't set procedure of instance.")

_allocate_instance(class, nfields) = 
  Instance(
    class,
    false,
    (args...)->error("An instance isn't a procedure -- can't apply it."),
    Vector{Any}(undef, nfields))

_allocate_callable = (class, nfields)->
  Instance(
    class,
    true,
    (args...)->error("Tried to call an entity before its proc is set."),
    Vector{Any}(undef, nfields))

#=
%allocate-instance, %allocate-entity, %instance-ref, %instance-set!
and class-of are the normal interface, from the rest of the code, to
the low-level memory system.  One thing to take note of is that the
protocol does not allow the user to add low-level instance
representations.  I have never seen a way to make that work.

Note that this implementation of class-of assumes the name of a the
primitive classes that are set up later.
=#

class_of = (x) ->
  is_instance(x) ?
    instance_class(x) :
    Top

#=
Now we can get down to business.  First, we initialize the braid.
For Bootstrapping, we define an early version of MAKE.  It will be
changed to the real version later on.  String search for ``set! make''.
=#

depth_first_cpl(class) =
  [class, foldl(vcat, map(depth_first_cpl, class_direct_superclasses(class)), init=[])...]

flavors_cpl(class) = 
  let base_cpl = [Object, Top]
    vcat(unique(filter(!in(base_cpl), depth_first_cpl(class))), base_cpl)
  end

loops_cpl(class) = 
  let base_cpl = [Object, Top]
    vcat(reverse(unique(reverse(filter(!in(base_cpl), depth_first_cpl(class))))), base_cpl)
  end

breath_first_cpl(class) = 
  let loop(sups, so_far) =
    sups == [] ?
      reverse(so_far) : 
      loop([sups[2:end]..., class_direct_superclasses(sups[1])...], 
           sups[1] in so_far ? so_far : [sups[1], so_far...])
    loop(class_direct_superclasses(class), [class])
end
# Temporary new, to be replaced later
new = (class; initargs...) ->
  if class === Class || class === Callable
    new = _allocate_instance(class, length(the_slots_of_a_class))
    name = get(initargs, :name, :anonymous_class)
    slot_set!(new, :name, name)
    dsupers = get(initargs, :direct_superclasses, [])
    slot_set!(new, :direct_superclasses, dsupers) # To support computing the CPL
    cpl = breath_first_cpl(new)
    slot_set!(new, :cpl, cpl)
    dslots = get(initargs, :direct_slots, [])
    slot_set!(new, :direct_slots, dslots)
    slots = vcat(dslots, map(class_direct_slots, cpl[2:end])...)
    slot_set!(new, :slots, slots)
    dinits = get(initargs, :direct_initializers, [()->missing for s in dslots])
    slot_set!(new, :direct_initializers, dinits)
    initializers = vcat(dinits, map(class_direct_initializers, cpl[2:end])...)
    slot_set!(new, :initializers, initializers)
    getters_and_setters = (; [slot=>((o)->_instance_ref(o, i), (o, n)->_instance_set!(o, i, n))
                            for (i, slot) in enumerate(slots)]...)
    slot_set!(new, :getters_and_setters, getters_and_setters)
    new
  elseif class === GenericFunction
    let new = _allocate_callable(class, length(class_slots(class))),
        name = get(initargs, :name, :anonymous_generic_function)
      slot_set!(new, :name, name)
      slot_set!(new, :methods, [])
      new
    end
  elseif class === MultiMethod
    let new = _allocate_instance(class, length(class_slots(class)))
      slot_set!(new, :specializers, get(initargs, :specializers, []))
      slot_set!(new, :procedure, get(initargs, :procedure, missing))
      new
    end
  end

#=
These are the real versions of slot-ref and slot-set!.  Because of the
way the new slot access protocol works, with no generic call in line,
they can be defined up front like this.  Cool eh?
=#

slot_ref(object, slot_name) = 
  let info = lookup_slot_info(class_of(object), slot_name),
      getter = info[1]
    getter(object)
  end

slot_set!(object, slot_name, new_value) =
  let info = lookup_slot_info(class_of(object), slot_name),
      setter = info[2]
    setter(object, new_value)
  end

lookup_slot_info(class, slot_name) = 
  let getters_and_setters = 
        (class === Class || class === Callable) ?
          getters_and_setters_for_class :
          slot_ref(class, :getters_and_setters),
      entry = get(getters_and_setters, slot_name, missing)
    ismissing(entry) ?
      error("No slot $slot_name in instances of $class") :
      entry
  end

#=
Given that the early version of MAKE is allowed to call accessors on
class metaobjects, the definitions for them come here, before the
actual class definitions, which are coming up right afterwards.
=#

class_name(class) = slot_ref(class, :name)
class_direct_slots(class) = slot_ref(class, :direct_slots)
class_slots(class) = slot_ref(class, :slots)
class_direct_initializers(class) = slot_ref(class, :direct_initializers)
class_initializers(class) = slot_ref(class, :initializers)
class_direct_superclasses(class) = slot_ref(class, :direct_superclasses)
class_cpl(class) = slot_ref(class, :cpl)
generic_methods(generic) = slot_ref(generic, :methods)
method_specializers(method) = slot_ref(method, :specializers)
method_procedure(method) = slot_ref(method, :procedure)

#=
The next 7 clusters define the 6 initial classes.  It takes 7 to 6
because the first and fourth both contribute to Class.
=#

the_slots_of_a_class = [
  :name,
  :direct_superclasses,              #(class ...)        
  :direct_slots,               #((name . options) ...)
  :direct_initializers,
  :cpl,                        #(class ...) 
  :slots,                      #((name . options) ...) 
  :initializers,               #(proc ...)
  :getters_and_setters]          #((slot-name getter setter) ...)

getters_and_setters_for_class =
  (; [s => (o->_instance_ref(o, f), (o, n)->_instance_set!(o, f, n)) for (f, s) in enumerate(the_slots_of_a_class)]...)

Class = _allocate_instance(missing, length(the_slots_of_a_class))
set_instance_class_to_self!(Class)


Top = new(Class, direct_superclasses=[], direct_slots=[], direct_initializers=[], name=:Top)
Object = new(Class, direct_superclasses=[Top], direct_slots=[], direct_initializers=[], name=:Object)

macro defclass0(name, metaclass, supers, slots)
    esc(:($name = 
      new($metaclass,
          direct_superclasses=$supers, 
          direct_slots=$slots,
          name=$(QuoteNode(name)))))
end

#=
This cluster, together with the first cluster above that defines
Class and sets its class, have the effect of:

Class = new(Class, direct_superclasses=[Object], direct_slots=[:direct_superclasses, ...])
=#
slot_set!(Class, :name, :Class)
slot_set!(Class, :direct_superclasses, [Object])
slot_set!(Class, :direct_slots, the_slots_of_a_class)
slot_set!(Class, :cpl, [Class, Object, Top])
slot_set!(Class, :slots, the_slots_of_a_class)
slot_set!(Class, :direct_initializers, [()->missing for s in the_slots_of_a_class])
slot_set!(Class, :initializers, [()->missing for s in the_slots_of_a_class])
slot_set!(Class, :getters_and_setters, (;))

@defclass0(ProcedureClass, Class, [Class], [:name])
@defclass0(Callable, Class, [ProcedureClass], [:name])
@defclass0(GenericFunction, Callable, [Object], [:name, :methods])
@defclass0(MultiMethod, Callable, [Object], [:specializers, :procedure, :generic_function])

#=
Base.show(io::IO, inst::Instance) = 
  let class = getfield(inst, :class)
    if class === Class || class === Callable || class === ProcedureClass
      print(io, "<$(slot_ref(inst, :name))>")
    elseif class === GenericFunction
      print(io, "<generic-function $(slot_ref(inst, :name)) with $(length(slot_ref(inst, :methods))) methods>")
    elseif class === MultiMethod
      print(io, "<method $(slot_ref(slot_ref(inst, :generic_function), :name)) for ($(slot_ref(inst, :specializers)...))>")
    else
      print(io, "<anonymous>")
    end
  end
=#

#=
These are the convenient syntax we expose to the base-level user.
=#
new_class(name, direct_superclasses, direct_slots) = new(Class, direct_superclasses=direct_superclasses, direct_slots=direct_slots, name=name)
new_generic(name) = new(GenericFunction, name=name)
new_method(specializers, procedure) = new(MultiMethod, specializers=specializers, procedure=procedure)

#=
The initialization protocol
=#
macro defgeneric(form, comb=:standard)
  let name = form.args[1],
      params = form.args[2:end],
      combination = Dict(:standard=>:StandardMethodCombination,
                         :and=>:AndMethodCombination,
                         :tuple=>:TupleMethodCombination)[comb]
    :($(esc(name)) = new(GenericFunction, name=$(QuoteNode(name))))
  end
end

@defgeneric initialize(obj, args)

#=
The instance structure protocol.
=#
@defgeneric allocate_instance(class)
@defgeneric compute_getter_and_setter(class)

#=
The class initialization protocol.
=#
@defgeneric compute_cpl(class)
@defgeneric compute_slots(class)
@defgeneric compute_initializers(class)

#=
The generic invocation protocol.
=#
@defgeneric compute_apply_generic(generic)
@defgeneric compute_methods(generic, args)
@defgeneric compute_is_method_more_specific()
@defgeneric compute_apply_methods()

#=
The next thing to do is bootstrap generic functions.
=# 
generic_invocation_generics = [compute_apply_generic,
                               compute_methods,
                               compute_is_method_more_specific,
                               compute_apply_methods]

add_method!(generic, method) = begin
  slot_set!(generic, :methods,
            [method,
             filter(m->method_specializers(m) != method_specializers(method),
                    slot_ref(generic, :methods))...])
  slot_set!(method, :generic_function, generic)
  set_instance_proc!(generic, compute_apply_generic(generic))
  method
end

#=
Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls
the other generics in the generic invocation protocol.  Two, related,
problems come up.  A chicken and egg problem and a infinite regress
problem.

In order to add our first method to COMPUTE-APPLY-GENERIC, we need
something sitting there, so it can be called.  The first definition
below does that.

Then, the second definition solves both the infinite regress and the
not having enough of the protocol around to build itself problem the
same way: it special cases invocation of generics in the invocation
protocol.
=#

set_instance_proc!(compute_apply_generic, generic->
  let method = generic_methods(generic)[1]
    method_procedure(method)(false, generic)
  end)

macro defmethod(form)
  esc(:(@defmethod(primary, $form)))
end
  
macro defmethod(role, form)
  let name = form.args[1].args[1],
      params = form.args[1].args[2:end],
      paramnames = [p isa Symbol ? p : p.args[1] for p in params],
      paramtypes = [p.args[2] for p in params if !(p isa Symbol)],
      body = form.args[2],
      role = Dict(:before=>:MethodRoleIsBefore,
                  :primary=>:MethodRoleIsPrimary,
                  :after=>:MethodRoleIsAfter)[role]
     esc(
      quote
        (@isdefined $name) || @defgeneric($name($paramnames...))
        add_method!($name, 
                     new(MultiMethod,
                          specializers=[$(paramtypes...)],
                          procedure=(call_next_method, $(paramnames...))->$body))
      end)
  end
end

@defmethod compute_apply_generic(generic::GenericFunction) = 
    (args...,)->generic in generic_invocation_generics && args[1] in generic_invocation_generics ? 
                 method_procedure(generic_methods(generic)[end])(false, args...) : 
                 compute_apply_methods(generic)(compute_methods(generic)(args), args)

@defmethod compute_methods(generic::GenericFunction) =
    (args)->let applicable = filter(method->all(((a, s),)->is_instance_of(a, s), zip(args, method_specializers(method))),
                                    generic_methods(generic)),
                is_more_specific = compute_is_method_more_specific(generic)
              isempty(applicable) ?
                no_applicable_method(generic, args) :
                sort(applicable, lt=(m1, m2)->is_more_specific(m1, m2, args))
            end

@defmethod compute_is_method_more_specific(generic::GenericFunction) =
  (m1, m2, args)->let loop(specls1, specls2, args) =
                        if specls1 == [] && specls2 == []
                          error("Two methods are equally specific.")
                        elseif specls1 == [] || specls2 == []
                          error("Two methods have a different number of specializers.")
                        elseif args == []
                          error("Fewer arguments than specializers.")
                        else
                          let (c1, c2, arg) = (specls1[1], specls2[1], args[1])
                            c1 === c2 ? 
                              loop(specls1[2:end], specls2[2:end], args[2:end]) : 
                              ismore_specific(c1, c2, arg)
                          end
                        end
                    loop(method_specializers(m1), method_specializers(m2), args)
                  end


@defmethod compute_apply_methods(generic::GenericFunction) =
  (methods, args)->let one_step(tail) = () ->
                         tail == [] ?
                           error("No next method.") : 
                           method_procedure(tail[1])(one_step(tail[2:end]), args...)
                     one_step(methods)()
                   end

isapplicable(c, arg) = begin
  ! isnothing(findfirst(==(c), class_cpl(class_of(arg))))
end

is_instance_of(arg, c) = ! isnothing(findfirst(==(c), class_cpl(class_of(arg))))

ismore_specific(c1, c2, arg) = 
  let cpl = class_cpl(class_of(arg)),
      i1 = findfirst(==(c1), cpl),
      i2 = findfirst(==(c2), cpl)
    i1 <= i2
  end

@defmethod no_applicable_method(generic::GenericFunction, args) =
  error("No applicable method for function $(generic.name) with arguments $args")

@defmethod initialize(object::Object, initargs) =
  for (key, val) in initargs
    slot_set!(object, key, val)
  end

@defmethod initialize(class::Class, initargs) = begin
  call_next_method()
  slot_set!(class, :direct_superclasses, get(initargs, :direct_superclasses, []))
  let direct_slots = get(initargs, :direct_slots, [])
    slot_set!(class, :direct_slots, direct_slots)
    slot_set!(class, :direct_initializers, get(initargs, :direct_initializers, [()->missing for s in direct_slots]))
    slot_set!(class, :cpl, compute_cpl(class))
    let slots = compute_slots(class)
      slot_set!(class, :slots, slots)
      slot_set!(class, :initializers, compute_initializers(class))
      slot_set!(class, :getters_and_setters, 
                              (; [slot=>compute_getter_and_setter(class, slot, i)
                                  for (i, slot) in enumerate(slots)]...))
    end
  end
end

@defmethod initialize(generic::GenericFunction, initargs) = begin
  call_next_method()
  slot_set!(generic, :methods, [])
  set_instance_proc!(generic, (args...)->error("Has no methods."))
end

@defmethod initialize(method::MultiMethod, initargs) = begin
  call_next_method()
  slot_set!(method, :specializers, get(initargs, :specializers, []))
  slot_set!(method, :procedure, initargs[:procedure])
end

@defmethod allocate_instance(class::Class) =
 let initializers = slot_ref(class, :initializers),
     new = _allocate_instance(class, length(initializers))
   for (i, init) in enumerate(initializers)
     _instance_set!(new, i, init())
   end
   new
 end

@defmethod allocate_instance(class::Callable) =
  let initializers = slot_ref(class, :initializers),
      new = _allocate_callable(class, length(initializers))
    for (i, init) in enumerate(initializers)
      _instance_set!(new, i, init())
    end
    new
  end


@defmethod compute_cpl(class::Class) =
  breath_first_cpl(class)

@defmethod compute_slots(class::Class) =
  vcat(map(class_direct_slots, class_cpl(class))...)

@defmethod compute_initializers(class::Class) =
  vcat(map(class_direct_initializers, class_cpl(class))...)

@defmethod compute_getter_and_setter(class::Class, slot, idx) =
  ((o)->_instance_ref(o, idx), (o, n)->_instance_set!(o, idx, n))

#=
Now everything works, both generic functions and classes, so we can
turn on the real MAKE.
=#
new = (class; initargs...) ->
  let instance = allocate_instance(class)
    initialize(instance, initargs)
    instance
  end

# Bootstrap print_object
@defgeneric print_object(obj, io)
@defmethod print_object(top::Top, io) =
  print(io, "<>")
@defmethod print_object(class::Class, io) =
  print(io, "<$(class_name(class_of(class))) $(class_name(class))>")
@defmethod print_object(obj::Object, io) =
  print(io, "<$(class_name(class_of(obj))) $(string(objectid(obj), base=62))>")
@defmethod print_object(gf::GenericFunction, io) =
  print(io, "<$(class_name(class_of(gf))) $(slot_ref(gf, :name)) with $(length(slot_ref(gf, :methods))) methods>")
@defmethod print_object(m::MultiMethod, io) =
  print(io, "<$(class_name(class_of(m))) $(slot_ref(slot_ref(m, :generic_function), :name))($(join(map(string ∘ class_name, slot_ref(m, :specializers)), ", ")))>")

# Connect this to Julia's native show.
Base.show(io::IO, inst::Instance) = print_object(inst, io)

class_of = (x) ->
  if is_instance(x)
    instance_class(x)
  else
    Top
  end

macro defclass(name, supers, slots, extra...)
  let maybe_expand_assign(s) =
        s.args[1] isa Expr && s.args[1].head === :(=) ?
          :([$(s.args[1].args[1]), initform=$(s.args[1].args[2]), $(s.args[2:end]...)]) :
          any(e->e isa Expr && e.head === :(=) && e.args[1] === :init, s.args[2:end]) ?
            s :
            :([$(s.args...), initform=missing]),
      slots = [s isa Expr && s.head === :vect ? s : :([$s]) for s in slots.args], # vectorize isolated slots
      #slots = [s.args[1] isa Symbol ? :([$(s.args[1])=missing, $(s.args[2:end]...)]) : s for s in slots], # add inits
      slots = map(maybe_expand_assign, slots), # add initforms
      slots = slots, # expand setters TODO
      # Now, we can extract the info
      inits = [:(()->$(opt.args[2]))
               for s in slots for opt in s.args[2:end] if opt.args[1] ===:initform],
      readers = [:(@defmethod $(opt.args[2])(o :: $(name)) = slot_ref(o, $(QuoteNode(s.args[1])))) 
                 for s in slots for opt in s.args[2:end] if opt.args[1] ===:reader],
      writers = [:(@defmethod $(opt.args[2])(o :: $(name), v) = slot_set!(o, $(QuoteNode(s.args[1])), v))
                 for s in slots for opt in s.args[2:end] if opt.args[1] ===:writer],
      slots = [QuoteNode(s.args[1]) for s in slots],
      metaclass = isempty(extra) ? :Class : (extra[1].args[1]===:metaclass ? extra[1].args[2] : error("Unknown option $(extra[1].args[1])")),
      supers = isempty(supers.args) ? [:Object] : [supers.args...]
    esc(quote
          global $name = 
            new($metaclass,
                direct_superclasses=[$(supers...)], 
                direct_slots=[$(slots...)],
                direct_initializers=[$(inits...)],
                name=$(QuoteNode(name)))
            $(readers...)
            $(writers...)
            $name
        end)
  end
end

export
  is_instance, instance_class, instance_slots,
  class_of,
  depth_first_cpl, flavors_cpl, loops_cpl, breath_first_cpl,
  slot_ref, slot_set!,
  lookup_slot_info, 
  class_name, 
  class_direct_slots, 
  class_slots, 
  class_direct_initializers, 
  class_initializers, 
  class_direct_superclasses, 
  class_cpl, 
  generic_methods, 
  method_specializers, 
  method_procedure, 
  Class, Top, Object, ProcedureClass,  Callable, GenericFunction, MultiMethod,
  new_class, new_generic, new_method, add_method!, set_instance_proc!,
  compute_apply_generic,
  compute_methods,
  compute_is_method_more_specific,
  compute_apply_methods,
  isapplicable,
  is_instance_of,
  initialize,
  allocate_instance,
  compute_cpl,
  compute_slots,
  compute_initializers,
  compute_getter_and_setter,
  new,
  @defgeneric, 
  @defmethod, 
  @defclass,
  print_object,
  call_next_method
