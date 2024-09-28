#include "clips.h"
#include "ruby.h"

size_t deffacts_size(const void *data)
{
	return sizeof(Deffacts);
}

static const rb_data_type_t Deffacts_type = {
	.function = {
		.dsize = deffacts_size
	},
	.flags = RUBY_TYPED_FREE_IMMEDIATELY
};

size_t deftemplate_size(const void *data)
{
	return sizeof(Deftemplate);
}

static const rb_data_type_t Deftemplate_type = {
	.function = {
		.dsize = deftemplate_size
	},
	.flags = RUBY_TYPED_FREE_IMMEDIATELY
};

size_t defmodule_size(const void *data)
{
	return sizeof(Defmodule);
}

static const rb_data_type_t Defmodule_type = {
	.function = {
		.dsize = defmodule_size
	},
	.flags = RUBY_TYPED_FREE_IMMEDIATELY
};

size_t defrule_size(const void *data)
{
	return sizeof(Defrule);
}

static const rb_data_type_t Defrule_type = {
	.function = {
		.dsize = defrule_size
	},
	.flags = RUBY_TYPED_FREE_IMMEDIATELY
};

size_t defclass_size(const void *data)
{
	return sizeof(Defclass);
}

static const rb_data_type_t Defclass_type = {
	.function = {
		.dsize = defclass_size
	},
	.flags = RUBY_TYPED_FREE_IMMEDIATELY
};

size_t fact_size(const void *data)
{
	return sizeof(Fact);
}

static const rb_data_type_t Fact_type = {
	.function = {
		.dsize = fact_size
	},
	.flags = RUBY_TYPED_FREE_IMMEDIATELY
};

size_t instance_size(const void *data)
{
	return sizeof(Instance);
}

static const rb_data_type_t Instance_type = {
	.function = {
		.dsize = instance_size
	},
	.flags = RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE clips_environment_fact_deftemplate(VALUE self)
{
	VALUE rbDeftemplate, rbEnvironment;
	Fact *fact;
	Deftemplate *template;

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	template = FactDeftemplate(fact);
	rbEnvironment = rb_iv_get(self, "@environment");
	rbDeftemplate = TypedData_Wrap_Struct(rb_const_get(rb_obj_class(rbEnvironment), rb_intern("Deftemplate")), &Deftemplate_type, template);
	rb_iv_set(rbDeftemplate, "@environment", rbEnvironment);
	return rbDeftemplate;
}

static VALUE clips_environment_fact_static_deftemplate(VALUE self, VALUE rbFact)
{
	return clips_environment_fact_deftemplate(rbFact);
}

static VALUE clips_environment_fact_deftemplate_name(VALUE self)
{
	Fact *fact;

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	return ID2SYM(rb_intern(DeftemplateName(FactDeftemplate(fact))));
}

static VALUE clips_environment_fact_static_deftemplate_name(VALUE self, VALUE rbFact)
{
	return clips_environment_fact_deftemplate_name(rbFact);
}

static VALUE clips_environment_deftemplate_defmodule_name(VALUE self)
{
	Deftemplate *template;

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, template);

	return ID2SYM(rb_intern(DeftemplateModule(template)));
}

static VALUE clips_environment_deftemplate_static_defmodule_name(VALUE self, VALUE rbDeftemplate)
{
	return clips_environment_deftemplate_defmodule_name(rbDeftemplate);
}

static VALUE clips_environment_defrule_defmodule_name(VALUE self)
{
	Defrule *rule;

	TypedData_Get_Struct(self, Defrule, &Defrule_type, rule);

	return ID2SYM(rb_intern(DefruleModule(rule)));
}

static VALUE clips_environment_defrule_static_defmodule_name(VALUE self, VALUE rbDefrule)
{
	return clips_environment_defrule_defmodule_name(rbDefrule);
}

void environment_free(void *data)
{
	DestroyEnvironment((Environment*) data);
}

size_t environment_size(const void *data)
{
	return MemUsed((Environment*) data);
}

static const rb_data_type_t Environment_type = {
	.function = {
		.dfree = environment_free,
		.dsize = environment_size
	},
	.flags = RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE clips_environment_facts(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Facts(env, "stdout", NULL, -1, -1, -1);

	return self;
}

static VALUE clips_environment_static_facts(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_facts(rbEnvironment);
}

VALUE defmodule_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Defmodule_type, NULL);
}

VALUE deffacts_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Deffacts_type, NULL);
}

VALUE deftemplate_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Deftemplate_type, NULL);
}

VALUE defclass_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Defclass_type, NULL);
}

VALUE defrule_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Defrule_type, NULL);
}

VALUE fact_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Fact_type, NULL);
}

VALUE instance_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Instance_type, NULL);
}

VALUE environment_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Environment_type, CreateEnvironment());
}

static VALUE create_environment(VALUE self)
{
	return environment_alloc(rb_const_get(self, rb_intern("Environment")));
}

static VALUE clips_environment_assert_string(VALUE self, VALUE string)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Fact *fact = AssertString(env, StringValueCStr(string));

	if (fact == NULL) {
		return Qnil;
	}

	VALUE rb_fact =
		TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Fact")), &Fact_type, fact);

	rb_iv_set(rb_fact, "@environment", self);

	return rb_fact;
}

static VALUE clips_environment_static_assert_string(VALUE self, VALUE rbEnvironment, VALUE string)
{
	return clips_environment_assert_string(rbEnvironment, string);
}

static VALUE clips_environment_defclass_name(VALUE self)
{
	Defclass *defclass;

	TypedData_Get_Struct(self, Defclass, &Defclass_type, defclass);

	return ID2SYM(rb_intern(DefclassName(defclass)));
}

static VALUE clips_environment_defclass_static_name(VALUE self, VALUE rbDefclass)
{
	return clips_environment_defclass_name(rbDefclass);
}

static VALUE clips_environment_defclass_defmodule_name(VALUE self)
{
	Defclass *defclass;

	TypedData_Get_Struct(self, Defclass, &Defclass_type, defclass);

	return ID2SYM(rb_intern(DefclassModule(defclass)));
}

static VALUE clips_environment_defclass_static_defmodule_name(VALUE self, VALUE rbDefclass)
{
	return clips_environment_defclass_defmodule_name(rbDefclass);
}

static VALUE clips_environment_defclass_pp_form(VALUE self)
{
	Defclass *defclass;
	const char *pp_form;

	TypedData_Get_Struct(self, Defclass, &Defclass_type, defclass);

	pp_form = DefclassPPForm(defclass);
	if (pp_form == NULL) {
		return Qnil;
	} else {
		return rb_str_new2(pp_form);
	}
}

static VALUE clips_environment_defclass_static_pp_form(VALUE self, VALUE rbDefclass)
{
	return clips_environment_defclass_pp_form(rbDefclass);
}

static VALUE clips_environment_defclass_get_instance_list(int argc, VALUE *argv, VALUE rbDefclass)
{
	VALUE rbEnvironment, rbInstance, include_subclasses, returnArray;
	Defclass *defclass;
	Instance *theInstance;
	UDFValue iterator;

	rbEnvironment = rb_iv_get(rbDefclass, "@environment");

	returnArray = rb_ary_new2(0);

	rb_scan_args(argc, argv, "01", &include_subclasses);

	TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);

	if (include_subclasses != Qtrue) {
		for (
			theInstance = GetNextInstanceInClass(defclass,NULL);
			theInstance != NULL;
			theInstance = GetNextInstanceInClass(defclass,theInstance)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	} else {
		for (
			theInstance = GetNextInstanceInClassAndSubclasses(&defclass,NULL,&iterator);
			theInstance != NULL;
			theInstance = GetNextInstanceInClassAndSubclasses(&defclass,theInstance,&iterator)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	}
	return returnArray;
}

static VALUE clips_environment_defclass_static_get_instance_list(int argc, VALUE *argv, VALUE rbDefclass)
{
	VALUE rbEnvironment, rbInstance, include_subclasses, returnArray;
	Defclass *defclass;
	Instance *theInstance;
	UDFValue iterator;

	returnArray = rb_ary_new2(0);

	rb_scan_args(argc, argv, "11", &rbDefclass, &include_subclasses);

	rbEnvironment = rb_iv_get(rbDefclass, "@environment");

	TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);

	if (include_subclasses != Qtrue) {
		for (
			theInstance = GetNextInstanceInClass(defclass,NULL);
			theInstance != NULL;
			theInstance = GetNextInstanceInClass(defclass,theInstance)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	} else {
		for (
			theInstance = GetNextInstanceInClassAndSubclasses(&defclass,NULL,&iterator);
			theInstance != NULL;
			theInstance = GetNextInstanceInClassAndSubclasses(&defclass,theInstance,&iterator)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	}
	return returnArray;
}


static VALUE clips_environment_make_instance(VALUE self, VALUE string)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Instance *instance = MakeInstance(env, StringValueCStr(string));

	if (instance == NULL) {
		return Qnil;
	}

	VALUE rb_instance =
		TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Instance")), &Instance_type, instance);

	rb_iv_set(rb_instance, "@environment", self);

	return rb_instance;
}

static VALUE clips_environment_static_make_instance(VALUE self, VALUE rbEnvironment, VALUE string)
{
	return clips_environment_make_instance(rbEnvironment, string);
}

static VALUE clips_environment_instance_unmake(VALUE self)
{
	Instance *instance;

	TypedData_Get_Struct(self, Instance, &Instance_type, instance);

	switch (UnmakeInstance(instance)) {
		case UIE_NO_ERROR:
			break;
		case UIE_NULL_POINTER_ERROR:
			rb_warn("could not unmake instance; null pointer error. This could be a bug in clipsruby!");
			return Qfalse;
		case UIE_COULD_NOT_DELETE_ERROR:
			rb_warn("could not unmake instance! is pattern matching currently happening with this instance?");
			return Qfalse;
		case UIE_DELETED_ERROR:
			rb_warn("could not unmake instance! instance is already deleted");
			return Qfalse;
		case UIE_RULE_NETWORK_ERROR:
			rb_warn("could not unmake instance! An error occurs while the unmaking was being processed in the rule network");
			return Qfalse;
        }

	return Qtrue;
}

static VALUE clips_environment_instance_static_unmake(VALUE self, VALUE rbInstance)
{
	return clips_environment_instance_unmake(rbInstance);
}

static VALUE clips_environment_instance_delete(VALUE self)
{
	Instance *instance;

	TypedData_Get_Struct(self, Instance, &Instance_type, instance);

	switch (DeleteInstance(instance)) {
		case UIE_NO_ERROR:
			break;
		case UIE_NULL_POINTER_ERROR:
			rb_warn("could not delete instance; null pointer error. This could be a bug in clipsruby!");
			return Qfalse;
		case UIE_COULD_NOT_DELETE_ERROR:
			rb_warn("could not delete instance! is pattern matching currently happening with this instance?");
			return Qfalse;
		case UIE_DELETED_ERROR:
			rb_warn("could not delete instance! instance is already deleted");
			return Qfalse;
		case UIE_RULE_NETWORK_ERROR:
			rb_warn("could not delete instance! An error occurs while the unmaking was being processed in the rule network");
			return Qfalse;
        }

	return Qtrue;
}

static VALUE clips_environment_instance_static_delete(VALUE self, VALUE rbInstance)
{
	return clips_environment_instance_delete(rbInstance);
}

static VALUE clips_environment_instance_defclass(VALUE self)
{
	Defclass *defclass;
	Instance *instance;
	VALUE rbEnvironment, rb_defclass;

	rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Instance, &Instance_type, instance);

	defclass = InstanceClass(instance);

	rb_defclass =
		TypedData_Wrap_Struct(rb_const_get(rb_obj_class(rbEnvironment), rb_intern("Defclass")), &Defclass_type, defclass);

	rb_iv_set(rb_defclass, "@environment", rbEnvironment);

	return rb_defclass;
}

static VALUE clips_environment_instance_static_defclass(VALUE self, VALUE rbInstance)
{
	return clips_environment_instance_defclass(rbInstance);
}

static VALUE clips_environment_instance_name(VALUE self)
{
	Instance *instance;

	TypedData_Get_Struct(self, Instance, &Instance_type, instance);

	return ID2SYM(rb_intern(InstanceName(instance)));
}

static VALUE clips_environment_instance_static_name(VALUE self, VALUE rbInstance)
{
	return clips_environment_instance_name(rbInstance);
}

static VALUE clips_environment_instance_pp_form(VALUE self)
{
	Environment *env;
	Instance *instance;
	StringBuilder *sb;
	VALUE toReturn;

	VALUE rbEnvironment = rb_iv_get(self, "@environment");
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	sb = CreateStringBuilder(env, 0);

	TypedData_Get_Struct(self, Instance, &Instance_type, instance);

	InstancePPForm(instance, sb);
	toReturn = rb_str_new2(sb->contents);
	SBDispose(sb);

	return toReturn;
}

static VALUE clips_environment_instance_static_pp_form(VALUE self, VALUE rbInstance)
{
	return clips_environment_instance_pp_form(rbInstance);
}

static VALUE clips_environment_build(VALUE self, VALUE string)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	switch(Build(env, StringValueCStr(string)))
	{
		case BE_NO_ERROR:
			break;
		case BE_COULD_NOT_BUILD_ERROR:
			rb_warn("`build` failed!");
			break;
		case BE_CONSTRUCT_NOT_FOUND_ERROR:
			rb_warn("`build` failed! Construct not found.");
			break;
		case BE_PARSING_ERROR:
			rb_warn("`build` failed! Could not parse string correctly.");
			break;
	}

	return Qnil;
}

static VALUE clips_environment_static_build(VALUE self, VALUE rbEnvironment, VALUE string)
{
	return clips_environment_build(rbEnvironment, string);
}

static CLIPSValue VALUE_to_CLIPSValue(VALUE from, Environment *env)
{
	CLIPSValue to;
	switch (TYPE(from))
	{
		case T_NIL:
			to.lexemeValue = CreateSymbol(env, "nil");
			break;
		case T_OBJECT:
			rb_warn("Ruby Object not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_CLASS:
			rb_warn("Ruby Class not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_MODULE:
			rb_warn("Ruby Module not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_FLOAT:
			to.floatValue = CreateFloat(env, NUM2DBL(from));
			break;
		case T_STRING:
			to.lexemeValue = CreateString(env, StringValueCStr(from));
			break;
		case T_REGEXP:
			rb_warn("Ruby Regexp not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_ARRAY:
			long length = RARRAY_LEN(from);

			MultifieldBuilder *mb = CreateMultifieldBuilder(env, length);
			CLIPSValue inner;
			for (int i = 0; i < length; i++)
			{
				inner = VALUE_to_CLIPSValue(rb_ary_entry(from, i), env);
				MBAppend(mb, &inner);
			}
			to.multifieldValue = MBCreate(mb);

			MBDispose(mb);
			break;
		case T_HASH:
			rb_warn("Ruby Hash not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_STRUCT:
			rb_warn("Ruby Struct not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_BIGNUM:
			to.floatValue = CreateFloat(env, NUM2LONG(from));
			break;
		case T_FIXNUM:
			to.integerValue = CreateInteger(env, FIX2INT(from));
			break;
		case T_COMPLEX:
			rb_warn("Ruby Complex not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_RATIONAL:
			rb_warn("Ruby Rational not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_FILE:
			rb_warn("Ruby File not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_TRUE:
			to.lexemeValue = CreateBoolean(env, "TRUE");
			break;
		case T_FALSE:
			to.lexemeValue = CreateBoolean(env, "FALSE");
			break;
		case T_DATA:
			rb_warn("Ruby Data not supported as value in CLIPS!");
			to.voidValue = VoidConstant(env);
			break;
		case T_SYMBOL:
			to.lexemeValue = CreateSymbol(env, rb_id2name(SYM2ID(from)));
			break;
		default:
			rb_warn("Trying to convert unknown Ruby data type to CLIPSValue");
			to.voidValue = VoidConstant(env);
			break;
	}
	return to;
}

void handle_pse_error(int error, const char *cslot)
{
	switch (error)
	{
		case PSE_NO_ERROR:
			break;
		case PSE_CARDINALITY_ERROR:
			rb_warn("Could not set slot %s: %s", cslot, "slot/multislot mismatch");
			break;
		case PSE_SLOT_NOT_FOUND_ERROR:
			rb_warn("Could not set slot %s: %s", cslot, "slot not found");
			break;
		case PSE_TYPE_ERROR:
			rb_warn("Could not set slot %s: %s", cslot, "value violates type constraint");
			break;
		case PSE_RANGE_ERROR:
			rb_warn("Could not set slot %s: %s", cslot, "value violates range constraint");
			break;
		case PSE_ALLOWED_VALUES_ERROR:
			rb_warn("Could not set slot %s: %s", cslot, "value violates allowed values constraint");
			break;
		case PSE_ALLOWED_CLASSES_ERROR:
			rb_warn("Could not set slot %s: %s", cslot, "value violates allowed classes constraint");
			break;
		case PSE_NULL_POINTER_ERROR:
		case PSE_INVALID_TARGET_ERROR:
		case PSE_EVALUATION_ERROR:
		case PSE_RULE_NETWORK_ERROR:
			rb_warn("Could not set slot %s: %s", cslot, "possible bug in clipsruby!");
			break;
	};
}

static int _clips_environment_assert_hash(VALUE key, VALUE value, VALUE args)
{
	const char *cslot_name;
	switch(TYPE(key))
	{
		case T_SYMBOL:
			cslot_name = rb_id2name(SYM2ID(key));
			break;
		case T_STRING:
			cslot_name = StringValueCStr(key);
			break;
		default:
			rb_raise(rb_eTypeError, "Slot name must be a String or a Symbol");
			return ST_CONTINUE;
	}

	VALUE *fb_and_env = (VALUE*)args;
	FactBuilder *fb = (FactBuilder*) fb_and_env[0];
	Environment *env = (Environment*) fb_and_env[1];
	CLIPSValue cv = VALUE_to_CLIPSValue(value, env);
	handle_pse_error(FBPutSlot(fb, cslot_name, &cv), cslot_name);

	return ST_CONTINUE;
}

static VALUE clips_environment_assert_hash(VALUE self, VALUE deftemplate_name, VALUE hash)
{
	const char *cdeftemplate_name;
	switch(TYPE(deftemplate_name))
	{
		case T_SYMBOL:
			cdeftemplate_name = rb_id2name(SYM2ID(deftemplate_name));
			break;
		case T_STRING:
			cdeftemplate_name = StringValueCStr(deftemplate_name);
			break;
		default:
			rb_raise(rb_eTypeError, "First argument must be a String or a Symbol");
			break;
	}

	Environment *env;
	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	FactBuilder *fb = CreateFactBuilder(env, cdeftemplate_name);
	switch (FBError(env))
	{
		case FBE_NO_ERROR:
			break;
		case FBE_DEFTEMPLATE_NOT_FOUND_ERROR:
			rb_warn("Could not assert fact; deftemplate not found!");
			return Qnil;
		case FBE_IMPLIED_DEFTEMPLATE_ERROR:
			rb_warn("Could not assert fact; cannot use assert_hash to create non-deftemplated facts!");
			return Qnil;
	}
	void *args[2] = { (void *)fb, (void *)env };
	rb_hash_foreach(hash, _clips_environment_assert_hash, (VALUE)args);
	Fact *fact = FBAssert(fb);
	FBDispose(fb);

	switch (FBError(env))
	{
		case FBE_NO_ERROR:
			break;
		case FBE_NULL_POINTER_ERROR:
			rb_warn("Could not assert fact. This might be a bug in clipsruby!");
			return Qnil;
		case FBE_COULD_NOT_ASSERT_ERROR:
			rb_warn("Could not assert fact. Pattern matching of a fact or instance is already occurring.");
			return Qnil;
		case FBE_RULE_NETWORK_ERROR:
			rb_warn("Could not assert fact. An error occurs while the assertion was being processed in the rule network.");
			return Qnil;
	}

	VALUE rb_fact =
		TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Fact")), &Fact_type, fact);

	rb_iv_set(rb_fact, "@environment", self);

	return rb_fact;
}

static VALUE clips_environment_static_assert_hash(VALUE self, VALUE environment, VALUE deftemplate_name, VALUE hash)
{
	return clips_environment_assert_hash(environment, deftemplate_name, hash);
}

static VALUE clips_environment_deftemplate_assert_hash(VALUE self, VALUE hash)
{
	const char *cdeftemplate_name;

	Environment *env;
	Deftemplate *template;

	VALUE rbEnvironment = rb_iv_get(self, "@environment");
	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, template);
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	cdeftemplate_name = DeftemplateName(template);

	FactBuilder *fb = CreateFactBuilder(env, cdeftemplate_name);
	switch (FBError(env))
	{
		case FBE_NO_ERROR:
			break;
		case FBE_DEFTEMPLATE_NOT_FOUND_ERROR:
			rb_warn("Could not assert fact; deftemplate not found!");
			return Qnil;
		case FBE_IMPLIED_DEFTEMPLATE_ERROR:
			rb_warn("Could not assert fact; cannot use assert_hash to create non-deftemplated facts!");
			return Qnil;
	}
	void *args[2] = { (void *)fb, (void *)env };
	rb_hash_foreach(hash, _clips_environment_assert_hash, (VALUE)args);
	Fact *fact = FBAssert(fb);
	FBDispose(fb);

	switch (FBError(env))
	{
		case FBE_NO_ERROR:
			break;
		case FBE_NULL_POINTER_ERROR:
			rb_warn("Could not assert fact. This might be a bug in clipsruby!");
			return Qnil;
		case FBE_COULD_NOT_ASSERT_ERROR:
			rb_warn("Could not assert fact. Pattern matching of a fact or instance is already occurring.");
			return Qnil;
		case FBE_RULE_NETWORK_ERROR:
			rb_warn("Could not assert fact. An error occurs while the assertion was being processed in the rule network.");
			return Qnil;
	}

	VALUE rb_fact =
		TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Fact")), &Fact_type, fact);

	rb_iv_set(rb_fact, "@environment", rbEnvironment);

	return rb_fact;
}

static VALUE clips_environment_deftemplate_static_assert_hash(VALUE self, VALUE deftemplate, VALUE hash)
{
	return clips_environment_deftemplate_assert_hash(deftemplate, hash);
}

static void CLIPSValue_to_VALUE(CLIPSValue *from, VALUE *value, VALUE *rbEnvironment)
{
	Environment *env;
	TypedData_Get_Struct(*rbEnvironment, Environment, &Environment_type, env);
	switch (from->header->type)
	{
		case VOID_TYPE:
			*value = Qnil;
			break;
		case FLOAT_TYPE:
			*value = DBL2NUM(from->floatValue->contents);
			break;
		case MULTIFIELD_TYPE:
			*value = rb_ary_new2(from->multifieldValue->length);
			for (size_t i = 0; i < from->multifieldValue->length; i++)
			{
				VALUE innerValue;
				CLIPSValue_to_VALUE(&from->multifieldValue->contents[i], &innerValue, rbEnvironment);
				rb_ary_push(*value, innerValue);
			}
			break;
		case SYMBOL_TYPE:
			if (from->lexemeValue == TrueSymbol(env)) {
				*value = Qtrue;
			} else if (from->lexemeValue == FalseSymbol(env)) {
				*value = Qfalse;
			} else {
				*value = ID2SYM(rb_intern(from->lexemeValue->contents));
			}
			break;
		case INTEGER_TYPE:
			*value = LONG2NUM(from->integerValue->contents);
			break;
		case STRING_TYPE:
		case INSTANCE_NAME_TYPE:
			*value = rb_str_new2(from->lexemeValue->contents);
			break;
		case FACT_ADDRESS_TYPE:
			*value =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(*rbEnvironment), rb_intern("Fact")), &Fact_type, from->factValue);

			rb_iv_set(*value, "@environment", *rbEnvironment);
			break;
		case INSTANCE_ADDRESS_TYPE:
			*value =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(*rbEnvironment), rb_intern("Instance")), &Instance_type, from->instanceValue);

			rb_iv_set(*value, "@environment", *rbEnvironment);
			break;
		case EXTERNAL_ADDRESS_TYPE:
		default:
			WriteString(env,STDERR,"Unsupported data type returned from function\n");
			*value = Qnil;
			break;
	}
}

static void UDFValue_to_VALUE(UDFValue *from, VALUE *value, VALUE *rbEnvironment)
{
	Environment *env;
	TypedData_Get_Struct(*rbEnvironment, Environment, &Environment_type, env);
	switch (from->header->type)
	{
		case VOID_TYPE:
			*value = Qnil;
			break;
		case FLOAT_TYPE:
			*value = DBL2NUM(from->floatValue->contents);
			break;
		case MULTIFIELD_TYPE:
			*value = rb_ary_new2(from->multifieldValue->length);
			VALUE innerValue;
			for (size_t i = 0; i < from->multifieldValue->length; i++)
			{
				CLIPSValue_to_VALUE(&from->multifieldValue->contents[i], &innerValue, rbEnvironment);
				rb_ary_push(*value, innerValue);
			}
			break;
		case SYMBOL_TYPE:
			if (from->lexemeValue == TrueSymbol(env)) {
				*value = Qtrue;
			} else if (from->lexemeValue == FalseSymbol(env)) {
				*value = Qfalse;
			} else {
				*value = ID2SYM(rb_intern(from->lexemeValue->contents));
			}
			break;
		case INTEGER_TYPE:
			*value = LONG2NUM(from->integerValue->contents);
			break;
		case STRING_TYPE:
		case INSTANCE_NAME_TYPE:
			*value = rb_str_new2(from->lexemeValue->contents);
			break;
		case FACT_ADDRESS_TYPE:
			*value =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(*rbEnvironment), rb_intern("Fact")), &Fact_type, from->factValue);

			rb_iv_set(*value, "@environment", *rbEnvironment);
			break;
		case INSTANCE_ADDRESS_TYPE:
			*value =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(*rbEnvironment), rb_intern("Instance")), &Instance_type, from->instanceValue);

			rb_iv_set(*value, "@environment", *rbEnvironment);
			break;
		case EXTERNAL_ADDRESS_TYPE:
		default:
			WriteString(env,STDERR,"Unsupported data type returned from function\n");
			*value = Qnil;
			break;
	}
}

void UDFGenericFunction(
		Environment *theEnv,
		UDFContext *context,
		UDFValue *returnValue)
{
	int argc = UDFArgumentCount(context);
	VALUE argv[argc];
	VALUE *current_argv = argv;
	UDFValue theArg;
	VALUE method = (VALUE)(context->context);
	VALUE rbEnvironment = rb_funcall(method, rb_intern("receiver"), 0);
	while (UDFHasNextArgument(context))
	{
		UDFNextArgument(context, ANY_TYPE_BITS, &theArg);
		UDFValue_to_VALUE(&theArg, current_argv, &rbEnvironment);
		current_argv++;
	}
	// Call the method on the object with the provided arguments
	VALUE result = rb_method_call(argc, argv, method);
	switch(TYPE(result))
	{
		case T_NIL:
			returnValue->voidValue = VoidConstant(theEnv);
			break;
		case T_OBJECT:
			InstanceBuilder *ib = CreateInstanceBuilder(theEnv, rb_obj_classname(result));
			// set instance properties here
			returnValue->instanceValue = IBMake(ib, NULL);
			IBDispose(ib);
			break;
		case T_CLASS:
		case T_MODULE:
			// return name of object or module so that CLIPS can make objects from it if it wanV
			returnValue->lexemeValue = CreateSymbol(theEnv, rb_class2name(result));
			break;
		case T_FLOAT:
			returnValue->floatValue = CreateFloat(theEnv, NUM2DBL(result));
			break;
		case T_STRING:
			returnValue->lexemeValue = CreateString(theEnv, StringValueCStr(result));
			break;
		case T_REGEXP:
			VALUE r = rb_funcall(result, rb_intern("source"), 0);
			returnValue->lexemeValue = CreateString(theEnv, StringValueCStr(r));
			break;
		case T_ARRAY:
			long length = RARRAY_LEN(result);

			MultifieldBuilder *mb = CreateMultifieldBuilder(theEnv, length);
			CLIPSValue inner;
			for (int i = 0; i < length; i++)
			{
				inner = VALUE_to_CLIPSValue(rb_ary_entry(result, i), theEnv);
				MBAppend(mb, &inner);
			}
			returnValue->multifieldValue = MBCreate(mb);

			MBDispose(mb);
			break;
		case T_SYMBOL:
			returnValue->lexemeValue = CreateSymbol(theEnv, rb_id2name(SYM2ID(result)));
			break;
		case T_FIXNUM:
		case T_BIGNUM:
			returnValue->integerValue = CreateInteger(theEnv, NUM2LONG(result));
			break;
		case T_TRUE:
			returnValue->lexemeValue = TrueSymbol(theEnv);
			break;
		case T_FALSE:
			returnValue->lexemeValue = FalseSymbol(theEnv);
			break;
		case T_HASH:
			//printf("Hash with size: %ld\n", RHASH_SIZE(obj));
			printf("Hash cannot be returned to CLIPS :(");
		case T_DATA:
		default:
			// unsupported data type
			WriteString(theEnv,STDERR,"Unsupported data type returned from function\n");
			perror("perror");
			break;
	}
}


static VALUE _clips_environment_add_udf(VALUE environment, VALUE method_name, VALUE clips_function_name) {
	Environment *env;
	TypedData_Get_Struct(environment, Environment, &Environment_type, env);

	// Look up the method entry for the given method name
	VALUE method = rb_obj_method(environment, method_name);
	if (!method) {
		rb_raise(rb_eNameError, "method '%s' not found", rb_id2name(method_name));
		return Qnil; // Will not be reached due to rb_raise
	}

	// Determine the arity of the method
	int min_args, max_args;
	int arity = rb_obj_method_arity(environment, SYM2ID(method_name));

	if (arity >= 0) {
		min_args = max_args = arity;
	} else {
		// Variable number of arguments
		min_args = -(arity + 1);
		max_args = UNBOUNDED;
	}

	switch (
		AddUDF(env,rb_id2name(SYM2ID(clips_function_name)),"*",min_args,max_args,"*",UDFGenericFunction,"UDFGenericFunction",(void*)method))
	{
		case AUE_MIN_EXCEEDS_MAX_ERROR:
		case AUE_FUNCTION_NAME_IN_USE_ERROR:
		case AUE_INVALID_ARGUMENT_TYPE_ERROR:
		case AUE_INVALID_RETURN_TYPE_ERROR:
			return Qfalse;
		case AUE_NO_ERROR:
		default:
			return Qtrue;
	}
}

static VALUE clips_environment_add_udf(int argc, VALUE *argv, VALUE environment) {
	VALUE method_name, clips_function_name;

	rb_scan_args(argc, argv, "11", &method_name, &clips_function_name);

	if (NIL_P(clips_function_name)) {
		clips_function_name = method_name;
	}

	return _clips_environment_add_udf(environment, method_name, clips_function_name);
}

static VALUE clips_environment_static_add_udf(int argc, VALUE *argv, VALUE klass) {
	VALUE environment, method_name, clips_function_name;

	rb_scan_args(argc, argv, "21", &environment, &method_name, &clips_function_name);

	if (NIL_P(clips_function_name)) {
		clips_function_name = method_name;
	}

	return _clips_environment_add_udf(environment, method_name, clips_function_name);
}

static VALUE clips_environment_deftemplate_slot_names(VALUE self)
{
	Deftemplate *template;
	Environment *env;
	CLIPSValue value;
	VALUE out;

	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, template);
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	DeftemplateSlotNames(template, &value);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_deftemplate_static_slot_names(VALUE self, VALUE rbDeftemplate)
{
	return clips_environment_deftemplate_slot_names(rbDeftemplate);
}

static VALUE clips_environment_defclass_superclasses(int argc, VALUE *argv, VALUE rbDefclass)
{
	VALUE rbEnvironment, inherit;
	Defclass *defclass;
	CLIPSValue value;
	VALUE out;

	rbEnvironment = rb_iv_get(rbDefclass, "@environment");

	rb_scan_args(argc, argv, "01", &inherit);

	if (NIL_P(inherit)) {
		inherit = Qfalse;
	}

	TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);

	ClassSuperclasses(defclass, &value, RTEST(inherit));

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_defclass_static_superclasses(int argc, VALUE *argv, VALUE klass)
{
	VALUE rbEnvironment, inherit, rbDefclass;
	Defclass *defclass;
	CLIPSValue value;
	VALUE out;

	rb_scan_args(argc, argv, "11", &rbDefclass, &inherit);

	TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);

	rbEnvironment = rb_iv_get(rbDefclass, "@environment");

	ClassSuperclasses(defclass, &value, RTEST(inherit));

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_defclass_subclasses(int argc, VALUE *argv, VALUE rbDefclass)
{
	VALUE rbEnvironment, inherit;
	Defclass *defclass;
	CLIPSValue value;
	VALUE out;

	rbEnvironment = rb_iv_get(rbDefclass, "@environment");

	rb_scan_args(argc, argv, "01", &inherit);

	if (NIL_P(inherit)) {
		inherit = Qfalse;
	}

	TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);

	ClassSubclasses(defclass, &value, RTEST(inherit));

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_defclass_static_subclasses(int argc, VALUE *argv, VALUE klass)
{
	VALUE rbEnvironment, inherit, rbDefclass;
	Defclass *defclass;
	CLIPSValue value;
	VALUE out;

	rb_scan_args(argc, argv, "11", &rbDefclass, &inherit);

	TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);

	rbEnvironment = rb_iv_get(rbDefclass, "@environment");

	ClassSubclasses(defclass, &value, RTEST(inherit));

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_run(int argc, VALUE *argv, VALUE environment) {
	VALUE integer;
	Environment *env;

	rb_scan_args(argc, argv, "01", &integer);
	if (NIL_P(integer)) {
		integer = INT2NUM(-1);
	}

	TypedData_Get_Struct(environment, Environment, &Environment_type, env);

	return INT2NUM(Run(env, NUM2INT(integer)));
}

static VALUE clips_environment_static_run(int argc, VALUE *argv, VALUE klass) {
	VALUE environment, integer;
	Environment *env;

	rb_scan_args(argc, argv, "11", &environment, &integer);
	if (NIL_P(integer)) {
		integer = INT2NUM(-1);
	}

	TypedData_Get_Struct(environment, Environment, &Environment_type, env);

	return INT2NUM(Run(env, NUM2INT(integer)));
}

static VALUE clips_environment_eval(VALUE self, VALUE string)
{
	Environment *env;
	CLIPSValue output;
	VALUE toReturn;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	switch(Eval(env, StringValueCStr(string), &output))
	{
		case EE_NO_ERROR:
			break;
		case EE_PROCESSING_ERROR:
			rb_warn("`eval` failed!");
			break;
		case EE_PARSING_ERROR:
			rb_warn("`eval` failed! Could not parse string correctly.");
			break;
	}

	CLIPSValue_to_VALUE(&output, &toReturn, &self);

	return toReturn;
}

static VALUE clips_environment_static_eval(VALUE self, VALUE rbEnvironment, VALUE string)
{
	return clips_environment_eval(rbEnvironment, string);
}

static VALUE clips_environment_find_fact(int argc, VALUE *argv, VALUE environment) {
	VALUE fact_set_template, query;

	rb_scan_args(argc, argv, "11", &fact_set_template, &query);
	if (NIL_P(query)) {
		query = rb_str_new_cstr("TRUE");
	}

	return clips_environment_eval(
		environment,
		rb_sprintf("(find-fact (%s) %s)", StringValueCStr(fact_set_template), StringValueCStr(query)));
}

static VALUE clips_environment_static_find_fact(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, fact_set_template, query;

	rb_scan_args(argc, argv, "21", &rbEnvironment, &fact_set_template, &query);
	if (NIL_P(query)) {
		query = rb_str_new_cstr("TRUE");
	}

	return clips_environment_eval(
		rbEnvironment,
		rb_sprintf("(find-fact (%s) %s)", StringValueCStr(fact_set_template), StringValueCStr(query)));
}

static VALUE clips_environment_find_all_facts(int argc, VALUE *argv, VALUE environment) {
	VALUE fact_set_template, query;

	rb_scan_args(argc, argv, "11", &fact_set_template, &query);
	if (NIL_P(query)) {
		query = rb_str_new_cstr("TRUE");
	}

	return clips_environment_eval(
		environment,
		rb_sprintf("(find-all-facts (%s) %s)", StringValueCStr(fact_set_template), StringValueCStr(query)));
}

static VALUE clips_environment_static_find_all_facts(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, fact_set_template, query;

	rb_scan_args(argc, argv, "21", &rbEnvironment, &fact_set_template, &query);
	if (NIL_P(query)) {
		query = rb_str_new_cstr("TRUE");
	}

	return clips_environment_eval(
		rbEnvironment,
		rb_sprintf("(find-all-facts (%s) %s)", StringValueCStr(fact_set_template), StringValueCStr(query)));
}

static VALUE clips_environment_fact_to_h(VALUE self)
{
	Fact *fact;
	CLIPSValue key, value;
	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	FactSlotNames(fact, &key);

	VALUE hash = rb_hash_new();
	for (size_t i = 0; i < key.multifieldValue->length; i++)
	{
		VALUE innerValue;
		GetFactSlot(fact, key.multifieldValue->contents[i].lexemeValue->contents, &value);
		CLIPSValue_to_VALUE(&value, &innerValue, &rbEnvironment);
		rb_hash_aset(hash, ID2SYM(rb_intern(key.multifieldValue->contents[i].lexemeValue->contents)), innerValue);
	}

	return hash;
}

static VALUE clips_environment_fact_static_to_h(VALUE self, VALUE rbFact)
{
	return clips_environment_fact_to_h(rbFact);
}

static VALUE clips_environment_batch_star(VALUE self, VALUE path)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	if (BatchStar(env, StringValueCStr(path))) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_static_batch_star(VALUE self, VALUE rbEnvironment, VALUE path)
{
	return clips_environment_batch_star(rbEnvironment, path);
}

static VALUE clips_environment_clear(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	if (Clear(env)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_static_clear(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_clear(rbEnvironment);
}

static VALUE clips_environment_reset(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Reset(env);

	return Qnil;
}

static VALUE clips_environment_static_reset(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_reset(rbEnvironment);
}

static VALUE clips_environment_save(VALUE self, VALUE path)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	if (Save(env, StringValueCStr(path))) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_static_save(VALUE self, VALUE rbEnvironment, VALUE path)
{
	return clips_environment_save(rbEnvironment, path);
}

static VALUE clips_environment_load(VALUE self, VALUE path)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	switch (Load(env, StringValueCStr(path)))
	{
		case LE_NO_ERROR:
			break;
		case LE_OPEN_FILE_ERROR:
			rb_warn("could not load: issue opening file.");
			return Qnil;
		case LE_PARSING_ERROR:
			rb_warn("could not load: parsing issue.");
			return Qnil;
	}
	return Qtrue;
}

static VALUE clips_environment_static_load(VALUE self, VALUE rbEnvironment, VALUE path)
{
	return clips_environment_load(rbEnvironment, path);
}

static VALUE clips_environment_save_facts(int argc, VALUE *argv, VALUE rbEnvironment) {
	VALUE path, scope;
	Environment *env;

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	rb_scan_args(argc, argv, "11", &path, &scope);
	if (NIL_P(scope)) {
		scope = ID2SYM(rb_intern("local"));
	}

	int number_of_facts_saved;

	if (scope == ID2SYM(rb_intern("visible"))) {
		number_of_facts_saved = SaveFacts(env, StringValueCStr(path), VISIBLE_SAVE);
	} else if (scope == ID2SYM(rb_intern("local"))) {
		number_of_facts_saved = SaveFacts(env, StringValueCStr(path), LOCAL_SAVE);
	} else {
		rb_warn("could not bsave_facts: unsupported scope.");
		return Qnil;
	}
	return INT2NUM(number_of_facts_saved);
}

static VALUE clips_environment_static_save_facts(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, path, scope;
	Environment *env;

	rb_scan_args(argc, argv, "21", &rbEnvironment, &path, &scope);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	if (NIL_P(scope)) {
		scope = ID2SYM(rb_intern("local"));
	}

	int number_of_facts_saved;

	if (scope == ID2SYM(rb_intern("visible"))) {
		number_of_facts_saved = SaveFacts(env, StringValueCStr(path), VISIBLE_SAVE);
	} else if (scope == ID2SYM(rb_intern("local"))) {
		number_of_facts_saved = SaveFacts(env, StringValueCStr(path), LOCAL_SAVE);
	} else {
		rb_warn("could not bsave_facts: unsupported scope.");
		return Qnil;
	}
	return INT2NUM(number_of_facts_saved);
}

static VALUE clips_environment_load_facts(VALUE self, VALUE path)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);
	int number_of_facts_loaded = LoadFacts(env, StringValueCStr(path));

	if (number_of_facts_loaded == -1) {
		return Qnil;
	} else {
		return INT2NUM(number_of_facts_loaded);
	}
}

static VALUE clips_environment_static_load_facts(VALUE self, VALUE rbEnvironment, VALUE path)
{
	return clips_environment_load_facts(rbEnvironment, path);
}

static VALUE clips_environment_bsave(VALUE self, VALUE path)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	if (Bsave(env, StringValueCStr(path))) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_static_bsave(VALUE self, VALUE rbEnvironment, VALUE path)
{
	return clips_environment_bsave(rbEnvironment, path);
}

static VALUE clips_environment_bload(VALUE self, VALUE path)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	if (Bload(env, StringValueCStr(path))) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_static_bload(VALUE self, VALUE rbEnvironment, VALUE path)
{
	return clips_environment_bload(rbEnvironment, path);
}

static VALUE clips_environment_bsave_facts(int argc, VALUE *argv, VALUE rbEnvironment) {
	VALUE path, scope;
	Environment *env;

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	rb_scan_args(argc, argv, "11", &path, &scope);
	if (NIL_P(scope)) {
		scope = ID2SYM(rb_intern("local"));
	}

	int number_of_facts_saved;

	if (scope == ID2SYM(rb_intern("visible"))) {
		number_of_facts_saved = BinarySaveFacts(env, StringValueCStr(path), VISIBLE_SAVE);
	} else if (scope == ID2SYM(rb_intern("local"))) {
		number_of_facts_saved = BinarySaveFacts(env, StringValueCStr(path), LOCAL_SAVE);
	} else {
		rb_warn("could not bsave_facts: unsupported scope.");
		return Qnil;
	}

	return INT2NUM(number_of_facts_saved);
}

static VALUE clips_environment_static_bsave_facts(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, path, scope;
	Environment *env;

	rb_scan_args(argc, argv, "21", &rbEnvironment, &path, &scope);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	if (NIL_P(scope)) {
		scope = ID2SYM(rb_intern("local"));
	}

	int number_of_facts_saved;

	if (scope == ID2SYM(rb_intern("visible"))) {
		number_of_facts_saved = BinarySaveFacts(env, StringValueCStr(path), VISIBLE_SAVE);
	} else if (scope == ID2SYM(rb_intern("local"))) {
		number_of_facts_saved = BinarySaveFacts(env, StringValueCStr(path), LOCAL_SAVE);
	} else {
		rb_warn("could not bsave_facts: unsupported scope.");
		return Qnil;
	}
	return INT2NUM(number_of_facts_saved);
}

static VALUE clips_environment_bload_facts(VALUE self, VALUE path)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);
	int number_of_facts_loaded = BinaryLoadFacts(env, StringValueCStr(path));

	if (number_of_facts_loaded == -1) {
		return Qnil;
	} else {
		return INT2NUM(number_of_facts_loaded);
	}
}

static VALUE clips_environment_static_bload_facts(VALUE self, VALUE rbEnvironment, VALUE path)
{
	return clips_environment_bload_facts(rbEnvironment, path);
}

static VALUE clips_environment_fact_slot_names(VALUE self)
{
	Fact *fact;
	CLIPSValue slot_names;
	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	FactSlotNames(fact, &slot_names);

	VALUE array;

	CLIPSValue_to_VALUE(&slot_names, &array, &rbEnvironment);

	return array;
}

static VALUE clips_environment_fact_static_slot_names(VALUE self, VALUE rbFact)
{
	return clips_environment_fact_slot_names(rbFact);
}

static VALUE clips_environment_fact_get_slot(VALUE self, VALUE slot_name)
{
	Fact *fact;
	CLIPSValue slot_value;
	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	switch (TYPE(slot_name)) {
		case T_STRING:
			GetFactSlot(fact, StringValueCStr(slot_name), &slot_value);
			break;
		case T_SYMBOL:
			GetFactSlot(fact, rb_id2name(SYM2ID(slot_name)), &slot_value);
			break;
                default:
			rb_warn("slot name must be string or symbol in call to get_slot!");
			return Qnil;
        }

	VALUE out;

	CLIPSValue_to_VALUE(&slot_value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_fact_static_get_slot(VALUE self, VALUE rbFact, VALUE slot_name)
{
	return clips_environment_fact_get_slot(rbFact, slot_name);
}

static VALUE clips_environment_deftemplate_slot_allowed_values(VALUE self, VALUE slot_name)
{
	Deftemplate *template;
	CLIPSValue slot_allowed_values;
	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, template);

	switch (TYPE(slot_name)) {
		case T_STRING:
			DeftemplateSlotAllowedValues(template, StringValueCStr(slot_name), &slot_allowed_values);
			break;
		case T_SYMBOL:
			DeftemplateSlotAllowedValues(template, rb_id2name(SYM2ID(slot_name)), &slot_allowed_values);
			break;
                default:
			rb_warn("slot name must be string or symbol in call to slot_allowed_values!");
			return Qnil;
        }

	VALUE out;

	CLIPSValue_to_VALUE(&slot_allowed_values, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_deftemplate_static_slot_allowed_values(VALUE self, VALUE rbDeftemplate, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_allowed_values(rbDeftemplate, slot_name);
}

static VALUE clips_environment_deftemplate_slot_default_value(VALUE self, VALUE slot_name)
{
	Deftemplate *template;
	CLIPSValue slot_default_value;
	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, template);

	switch (TYPE(slot_name)) {
		case T_STRING:
			DeftemplateSlotDefaultValue(template, StringValueCStr(slot_name), &slot_default_value);
			break;
		case T_SYMBOL:
			DeftemplateSlotDefaultValue(template, rb_id2name(SYM2ID(slot_name)), &slot_default_value);
			break;
                default:
			rb_warn("slot name must be string or symbol in call to slot_default_value!");
			return Qnil;
        }

	VALUE out;

	CLIPSValue_to_VALUE(&slot_default_value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_deftemplate_static_slot_default_value(VALUE self, VALUE rbDeftemplate, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_default_value(rbDeftemplate, slot_name);
}


static VALUE clips_environment_deftemplate_slot_types(VALUE self, VALUE slot_name)
{
	Deftemplate *template;
	CLIPSValue slot_types;
	VALUE out;
	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, template);

	switch (TYPE(slot_name)) {
		case T_STRING:
			DeftemplateSlotTypes(template, StringValueCStr(slot_name), &slot_types);
			break;
		case T_SYMBOL:
			DeftemplateSlotTypes(template, rb_id2name(SYM2ID(slot_name)), &slot_types);
			break;
                default:
			rb_warn("slot name must be string or symbol in call to slot_types!");
			return Qnil;
        }

	CLIPSValue_to_VALUE(&slot_types, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_deftemplate_static_slot_types(VALUE self, VALUE rbEnvironment, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_types(rbEnvironment, slot_name);
}

static VALUE clips_environment_deftemplate_slot_range(VALUE self, VALUE slot_name)
{
	Deftemplate *template;
	CLIPSValue slot_range;
	VALUE out;
	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, template);

	switch (TYPE(slot_name)) {
		case T_STRING:
			DeftemplateSlotRange(template, StringValueCStr(slot_name), &slot_range);
			break;
		case T_SYMBOL:
			DeftemplateSlotRange(template, rb_id2name(SYM2ID(slot_name)), &slot_range);
			break;
                default:
			rb_warn("slot name must be string or symbol in call to slot_range!");
			return Qnil;
        }

	CLIPSValue_to_VALUE(&slot_range, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_deftemplate_static_slot_range(VALUE self, VALUE rbEnvironment, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_range(rbEnvironment, slot_name);
}

static VALUE clips_environment_deftemplate_slot_cardinality(VALUE self, VALUE slot_name)
{
	Deftemplate *template;
	CLIPSValue slot_cardinality;
	VALUE out;
	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, template);

	switch (TYPE(slot_name)) {
		case T_STRING:
			DeftemplateSlotCardinality(template, StringValueCStr(slot_name), &slot_cardinality);
			break;
		case T_SYMBOL:
			DeftemplateSlotCardinality(template, rb_id2name(SYM2ID(slot_name)), &slot_cardinality);
			break;
                default:
			rb_warn("slot name must be string or symbol in call to slot_cardinality!");
			return Qnil;
        }

	CLIPSValue_to_VALUE(&slot_cardinality, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_deftemplate_static_slot_cardinality(VALUE self, VALUE rbEnvironment, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_cardinality(rbEnvironment, slot_name);
}

static VALUE clips_environment_fact_retract(VALUE self)
{
	Fact *fact;

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	switch (Retract(fact)) {
		case RE_NO_ERROR:
			break;
		case RE_NULL_POINTER_ERROR:
			rb_warn("could not retract fact; null pointer error. This could be a bug in clipsruby!");
			return Qfalse;
		case RE_COULD_NOT_RETRACT_ERROR:
			rb_warn("could not retract fact! is pattern matching currently happening with this fact?");
			return Qfalse;
		case RE_RULE_NETWORK_ERROR:
			rb_warn("could not retract fact! An error occurs while the retraction was being processed in the rule network");
			return Qfalse;
        }

	return Qtrue;
}

static VALUE clips_environment_fact_static_retract(VALUE self, VALUE rbFact)
{
	return clips_environment_fact_retract(rbFact);
}

static int _clips_environment_fact_modify(VALUE key, VALUE value, VALUE args)
{
	const char *cslot_name;
	switch(TYPE(key))
	{
		case T_SYMBOL:
			cslot_name = rb_id2name(SYM2ID(key));
			break;
		case T_STRING:
			cslot_name = StringValueCStr(key);
			break;
		default:
			rb_raise(rb_eTypeError, "Slot name must be a String or a Symbol");
			return ST_CONTINUE;
	}

	VALUE *fm_and_env = (VALUE*)args;
	FactModifier *fm = (FactModifier*) fm_and_env[0];
	Environment *env = (Environment*) fm_and_env[1];
	CLIPSValue cv = VALUE_to_CLIPSValue(value, env);
	handle_pse_error(FMPutSlot(fm, cslot_name, &cv), cslot_name);

	return ST_CONTINUE;
}

static VALUE clips_environment_fact_modify(VALUE self, VALUE hash)
{
	Fact *fact;
	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	VALUE rbEnvironment = rb_iv_get(self, "@environment");
	Environment *env;
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	FactModifier *fm = CreateFactModifier(env, fact);
	switch (FMError(env))
	{
		case FME_NO_ERROR:
			break;
		case FME_RETRACTED_ERROR:
			rb_warn("Could not modify fact; fact is retracted!");
			return Qnil;
		case FME_IMPLIED_DEFTEMPLATE_ERROR:
			rb_warn("Could not modify fact; cannot use modify to modify non-deftemplated facts!");
			return Qnil;
	}
	void *args[2] = { (void *)fm, (void *)env };
	rb_hash_foreach(hash, _clips_environment_fact_modify, (VALUE)args);
	Fact *modified_fact = FMModify(fm);
	FMDispose(fm);

	switch (FMError(env))
	{
		case FME_NO_ERROR:
			break;
		case FME_NULL_POINTER_ERROR:
			rb_warn("Could not modify fact. This might be a bug in clipsruby!");
			return Qnil;
		case FME_RETRACTED_ERROR:
			rb_warn("Could not modify fact; fact is retracted!");
			return Qnil;
		case FME_COULD_NOT_MODIFY_ERROR:
			rb_warn("Could not modify fact. Pattern matching of a fact or instance is already occurring.");
			return Qnil;
		case FME_RULE_NETWORK_ERROR:
			rb_warn("Could not modify fact. An error occurs while the assertion was being processed in the rule network.");
			return Qnil;
	}

	VALUE rb_fact =
		TypedData_Wrap_Struct(CLASS_OF(self), &Fact_type, modified_fact);

	rb_iv_set(rb_fact, "@environment", rbEnvironment);

	return rb_fact;
}

static VALUE clips_environment_fact_static_modify(VALUE self, VALUE rbFact, VALUE hash)
{
	return clips_environment_fact_modify(rbFact, hash);
}

static VALUE clips_environment_fact_index(VALUE self)
{
	Fact *fact;

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	return INT2NUM(FactIndex(fact));
}

static VALUE clips_environment_fact_static_index(VALUE self, VALUE rbFact)
{
	return clips_environment_fact_index(rbFact);
}

static VALUE clips_environment_find_defrule(VALUE self, VALUE defrule_name)
{
	Environment *env;
	Defrule *rule;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	switch (TYPE(defrule_name)) {
		case T_STRING:
			rule = FindDefrule(env, StringValueCStr(defrule_name));
			break;
		case T_SYMBOL:
			rule = FindDefrule(env, rb_id2name(SYM2ID(defrule_name)));
			break;
                default:
			rb_warn("defrule name must be a symbol or string");
			return Qnil;
        }

	if (rule == NULL) {
		return Qnil;
	} else {
		return TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Defrule")), &Defrule_type, rule);
	}
}

static VALUE clips_environment_static_find_defrule(VALUE self, VALUE rbEnvironment, VALUE defrule_name)
{
	return clips_environment_find_defrule(rbEnvironment, defrule_name);
}

static VALUE clips_environment_find_deffacts(VALUE self, VALUE deffacts_name)
{
	Environment *env;
	Deffacts *deffacts;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	switch (TYPE(deffacts_name)) {
		case T_STRING:
			deffacts = FindDeffacts(env, StringValueCStr(deffacts_name));
			break;
		case T_SYMBOL:
			deffacts = FindDeffacts(env, rb_id2name(SYM2ID(deffacts_name)));
			break;
                default:
			rb_warn("deffacts name must be a symbol or string");
			return Qnil;
        }

	if (deffacts == NULL) {
		return Qnil;
	} else {
		return TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Deffacts")), &Deffacts_type, deffacts);
	}
}

static VALUE clips_environment_static_find_deffacts(VALUE self, VALUE rbEnvironment, VALUE deffacts_name)
{
	return clips_environment_find_deffacts(rbEnvironment, deffacts_name);
}

static VALUE clips_environment_deffacts_name(VALUE self)
{
	Deffacts *deffacts;

	TypedData_Get_Struct(self, Deffacts, &Deffacts_type, deffacts);

	return ID2SYM(rb_intern(DeffactsName(deffacts)));
}

static VALUE clips_environment_deffacts_static_name(VALUE self, VALUE rbDeffacts)
{
	return clips_environment_deffacts_name(rbDeffacts);
}

static VALUE clips_environment_deffacts_pp_form(VALUE self)
{
	Deffacts *deffacts;

	TypedData_Get_Struct(self, Deffacts, &Deffacts_type, deffacts);

	return rb_str_new2(DeffactsPPForm(deffacts));
}

static VALUE clips_environment_deffacts_static_pp_form(VALUE self, VALUE rbDeffacts)
{
	return clips_environment_deffacts_pp_form(rbDeffacts);
}

static VALUE clips_environment_get_current_module(VALUE self)
{
	Environment *env;
	Defmodule *module;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);
	module = GetCurrentModule(env);

	if (module == NULL) {
		return Qnil;
	} else {
		VALUE rbDefmodule;
		rbDefmodule = TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Defmodule")), &Defmodule_type, module);
		rb_iv_set(rbDefmodule, "@environment", self);
		return rbDefmodule;
	}
}

static VALUE clips_environment_static_get_current_module(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_get_current_module(rbEnvironment);
}

static VALUE clips_environment_set_current_module(VALUE self, VALUE rbDefmodule)
{
	Environment *env;
	Defmodule *module;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);
	TypedData_Get_Struct(rbDefmodule, Defmodule, &Defmodule_type, module);

	SetCurrentModule(env, module);

	return rbDefmodule;
}

static VALUE clips_environment_static_set_current_module(VALUE self, VALUE rbEnvironment, VALUE defmodule_name)
{
	return clips_environment_set_current_module(rbEnvironment, defmodule_name);
}

static VALUE clips_environment_find_defmodule(VALUE self, VALUE defmodule_name)
{
	Environment *env;
	Defmodule *module;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	switch (TYPE(defmodule_name)) {
		case T_STRING:
			module = FindDefmodule(env, StringValueCStr(defmodule_name));
			break;
		case T_SYMBOL:
			module = FindDefmodule(env, rb_id2name(SYM2ID(defmodule_name)));
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }

	if (module == NULL) {
		return Qnil;
	} else {
		VALUE rbDefmodule;
		rbDefmodule = TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Defmodule")), &Defmodule_type, module);
		rb_iv_set(rbDefmodule, "@environment", self);
		return rbDefmodule;
	}
}

static VALUE clips_environment_static_find_defmodule(VALUE self, VALUE rbEnvironment, VALUE defmodule_name)
{
	return clips_environment_find_defmodule(rbEnvironment, defmodule_name);
}

static VALUE clips_environment_find_instance(int argc, VALUE *argv, VALUE rbEnvironment) {
	VALUE instanceName, search_imports, rbInstance, rbDefmodule;
	Defmodule *defmodule;
	Environment *env;
	Instance *instance;

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	rb_scan_args(argc, argv, "12", &instanceName, &rbDefmodule, &search_imports);

	const char *cinstanceName;
	switch(TYPE(instanceName))
	{
		case T_SYMBOL:
			cinstanceName = rb_id2name(SYM2ID(instanceName));
			break;
		case T_STRING:
			cinstanceName = StringValueCStr(instanceName);
			break;
		default:
			rb_warn("instance name must be a String or a Symbol");
			return Qnil;
	}

	switch (TYPE(rbDefmodule)) {
		case T_NIL:
			defmodule = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			rbDefmodule = clips_environment_find_defmodule(rbEnvironment, rbDefmodule);
			if (rbDefmodule == Qnil) {
				return Qnil;
			}
			TypedData_Get_Struct(rbDefmodule, Defmodule, &Defmodule_type, defmodule);
			break;
		case T_DATA:
			TypedData_Get_Struct(rbDefmodule, Defmodule, &Defmodule_type, defmodule);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }

	instance = FindInstance(
		env,
		defmodule,
		cinstanceName,
		RTEST(search_imports));

	if (instance == NULL) {
		return Qnil;
	} else {
		rbInstance =
			TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, instance);

		rb_iv_set(rbInstance, "@environment", rbEnvironment);

		return rbInstance;
	}
}

static VALUE clips_environment_static_find_instance(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, instanceName, rbInstance, rbDefmodule, search_imports;
	Defmodule *defmodule;
	Environment *env;
	Instance *instance;

	rb_scan_args(argc, argv, "22", &rbEnvironment, &instanceName, &rbDefmodule, &search_imports);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	const char *cinstanceName;
	switch(TYPE(instanceName))
	{
		case T_SYMBOL:
			cinstanceName = rb_id2name(SYM2ID(instanceName));
			break;
		case T_STRING:
			cinstanceName = StringValueCStr(instanceName);
			break;
		default:
			rb_warn("instance name must be a String or a Symbol");
			return Qnil;
	}

	switch (TYPE(rbDefmodule)) {
		case T_NIL:
			defmodule = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			rbDefmodule = clips_environment_find_defmodule(rbEnvironment, rbDefmodule);
			if (rbDefmodule == Qnil) {
				return Qnil;
			}
			TypedData_Get_Struct(rbDefmodule, Defmodule, &Defmodule_type, defmodule);
			break;
		case T_DATA:
			TypedData_Get_Struct(rbDefmodule, Defmodule, &Defmodule_type, defmodule);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }

	instance = FindInstance(
		env,
		defmodule,
		cinstanceName,
		RTEST(search_imports));

	if (instance == NULL) {
		return Qnil;
	} else {
		rbInstance =
			TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, instance);

		rb_iv_set(rbInstance, "@environment", rbEnvironment);

		return rbInstance;
	}
}

static VALUE clips_environment_defmodule_find_instance(int argc, VALUE *argv, VALUE rbDefmodule) {
	VALUE instanceName, search_imports, rbInstance, rbEnvironment;
	Defmodule *defmodule;
	Environment *env;
	Instance *instance;
	const char *cinstanceName;

	TypedData_Get_Struct(rbDefmodule, Defmodule, &Defmodule_type, defmodule);

	rbEnvironment = rb_iv_get(rbDefmodule, "@environment");

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	TypedData_Get_Struct(rbDefmodule, Defmodule, &Defmodule_type, defmodule);

	rb_scan_args(argc, argv, "11", &instanceName, &search_imports);

	switch(TYPE(instanceName))
	{
		case T_SYMBOL:
			cinstanceName = rb_id2name(SYM2ID(instanceName));
			break;
		case T_STRING:
			cinstanceName = StringValueCStr(instanceName);
			break;
		default:
			rb_warn("instance name must be a String or a Symbol");
			return Qnil;
	}

	instance = FindInstance(
		env,
		defmodule,
		cinstanceName,
		RTEST(search_imports));

	if (instance == NULL) {
		return Qnil;
	} else {
		rbInstance =
			TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, instance);

		rb_iv_set(rbInstance, "@environment", rbEnvironment);

		return rbInstance;
	}
}

static VALUE clips_environment_defmodule_static_find_instance(int argc, VALUE *argv, VALUE klass) {
	VALUE rbDefmodule, rbEnvironment, instanceName, search_imports, rbInstance;
	Defmodule *defmodule;
	Environment *env;
	Instance *instance;
	const char *cinstanceName;

	rb_scan_args(argc, argv, "21", &rbDefmodule, &instanceName, &search_imports);

	rbEnvironment = rb_iv_get(rbDefmodule, "@environment");

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	TypedData_Get_Struct(rbDefmodule, Defmodule, &Defmodule_type, defmodule);

	switch(TYPE(instanceName))
	{
		case T_SYMBOL:
			cinstanceName = rb_id2name(SYM2ID(instanceName));
			break;
		case T_STRING:
			cinstanceName = StringValueCStr(instanceName);
			break;
		default:
			rb_warn("instance name must be a String or a Symbol");
			return Qnil;
	}

	instance = FindInstance(
		env,
		defmodule,
		cinstanceName,
		RTEST(search_imports));

	if (instance == NULL) {
		return Qnil;
	} else {
		rbInstance =
			TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, instance);

		rb_iv_set(rbInstance, "@environment", rbEnvironment);

		return rbInstance;
	}
}


static VALUE clips_environment_find_deftemplate(VALUE self, VALUE deftemplate_name)
{
	Environment *env;
	Deftemplate *template;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	switch (TYPE(deftemplate_name)) {
		case T_STRING:
			template = FindDeftemplate(env, StringValueCStr(deftemplate_name));
			break;
		case T_SYMBOL:
			template = FindDeftemplate(env, rb_id2name(SYM2ID(deftemplate_name)));
			break;
                default:
			rb_warn("deftemplate name must be a symbol or string");
			return Qnil;
        }

	if (template == NULL) {
		return Qnil;
	} else {
		VALUE rbDeftemplate;
		rbDeftemplate = TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Deftemplate")), &Deftemplate_type, template);
		rb_iv_set(rbDeftemplate, "@environment", self);
		return rbDeftemplate;
	}
}

static VALUE clips_environment_static_find_deftemplate(VALUE self, VALUE rbEnvironment, VALUE deftemplate_name)
{
	return clips_environment_find_deftemplate(rbEnvironment, deftemplate_name);
}

static VALUE clips_environment_find_defclass(VALUE self, VALUE defclass_name)
{
	Environment *env;
	Defclass *class;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	switch (TYPE(defclass_name)) {
		case T_STRING:
			class = FindDefclass(env, StringValueCStr(defclass_name));
			break;
		case T_SYMBOL:
			class = FindDefclass(env, rb_id2name(SYM2ID(defclass_name)));
			break;
                default:
			rb_warn("defclass name must be a symbol or string");
			return Qnil;
        }

	if (class == NULL) {
		return Qnil;
	} else {
		VALUE rbDefclass;
		rbDefclass = TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Defclass")), &Defclass_type, class);
		rb_iv_set(rbDefclass, "@environment", self);
		return rbDefclass;
	}
}

static VALUE clips_environment_static_find_defclass(VALUE self, VALUE rbEnvironment, VALUE defclass_name)
{
	return clips_environment_find_defclass(rbEnvironment, defclass_name);
}

static VALUE clips_environment_deftemplate_name(VALUE self)
{
	Deftemplate *deftemplate;

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, deftemplate);

	return ID2SYM(rb_intern(DeftemplateName(deftemplate)));
}

static VALUE clips_environment_deftemplate_static_name(VALUE self, VALUE rbDeftemplate)
{
	return clips_environment_deftemplate_name(rbDeftemplate);
}

static VALUE clips_environment_deftemplate_pp_form(VALUE self)
{
	Deftemplate *deftemplate;

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, deftemplate);

	return rb_str_new2(DeftemplatePPForm(deftemplate));
}

static VALUE clips_environment_deftemplate_static_pp_form(VALUE self, VALUE rbDeftemplate)
{
	return clips_environment_deftemplate_pp_form(rbDeftemplate);
}

static VALUE clips_environment_fact_pp_form(int argc, VALUE *argv, VALUE self)
{
	Fact *fact;
	VALUE ignore_defaults, toReturn;
	Environment *env;

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);
	VALUE rbEnvironment = rb_iv_get(self, "@environment");
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	StringBuilder *sb = CreateStringBuilder(env, 0);

	rb_scan_args(argc, argv, "01", &ignore_defaults);

	if (ignore_defaults == Qtrue) {
		FactPPForm(fact, sb, true);
	} else {
		FactPPForm(fact, sb, false);
	}

	toReturn = rb_str_new2(sb->contents);

	SBDispose(sb);

	return toReturn;
}

static VALUE clips_environment_fact_static_pp_form(int argc, VALUE *argv, VALUE klass)
{
	Fact *fact;
	VALUE self, ignore_defaults, toReturn;
	Environment *env;

	rb_scan_args(argc, argv, "11", &self, &ignore_defaults);

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);
	VALUE rbEnvironment = rb_iv_get(self, "@environment");
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	StringBuilder *sb = CreateStringBuilder(env, 0);

	if (ignore_defaults == Qtrue) {
		FactPPForm(fact, sb, true);
	} else {
		FactPPForm(fact, sb, false);
	}

	toReturn = rb_str_new2(sb->contents);

	SBDispose(sb);

	return toReturn;
}

static VALUE clips_environment_fact_existp(VALUE self)
{
	Fact *fact;

	TypedData_Get_Struct(self, Fact, &Fact_type, fact);

	if (FactExistp(fact)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_fact_static_existp(VALUE self, VALUE rbFact)
{
	return clips_environment_fact_existp(rbFact);
}

static VALUE clips_environment_get_fact_list(int argc, VALUE *argv, VALUE rbEnvironment) {
	VALUE defmodule_or_defmodule_name;
	Environment *env;
	Defmodule *module;
	CLIPSValue value;
	VALUE out;

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	rb_scan_args(argc, argv, "01", &defmodule_or_defmodule_name);
	switch (TYPE(defmodule_or_defmodule_name)) {
		case T_NIL:
			module = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			TypedData_Get_Struct(
				clips_environment_find_defmodule(rbEnvironment, defmodule_or_defmodule_name),
				Defmodule, &Defmodule_type, module);
			break;
		case T_DATA:
			TypedData_Get_Struct(defmodule_or_defmodule_name, Defmodule, &Defmodule_type, module);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }
	GetFactList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_static_get_fact_list(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, defmodule_or_defmodule_name;
	Environment *env;
	Defmodule *module;
	CLIPSValue value;
	VALUE out;

	rb_scan_args(argc, argv, "11", &rbEnvironment, &defmodule_or_defmodule_name);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	switch (TYPE(defmodule_or_defmodule_name)) {
		case T_NIL:
			module = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			TypedData_Get_Struct(
				clips_environment_find_defmodule(rbEnvironment, defmodule_or_defmodule_name),
				Defmodule, &Defmodule_type, module);
			break;
		case T_DATA:
			TypedData_Get_Struct(defmodule_or_defmodule_name, Defmodule, &Defmodule_type, module);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }
	GetFactList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_get_instance_list(int argc, VALUE *argv, VALUE rbEnvironment) {
	VALUE rbDefclass_or_defclass_name, rbDefclass, include_subclasses, rbInstance, returnArray;
	Environment *env;
	Defclass *defclass;
	UDFValue iterator;
	Instance *theInstance;

	returnArray = rb_ary_new2(0);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	rb_scan_args(argc, argv, "02", &rbDefclass_or_defclass_name, &include_subclasses);
	// defclass is undefined, ignore include_subclasses
	if (rbDefclass_or_defclass_name == Qnil) {
		for (
			theInstance = GetNextInstance(env,NULL);
			theInstance != NULL;
			theInstance = GetNextInstance(env,theInstance)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	} else if (include_subclasses != Qtrue) {
		switch (TYPE(rbDefclass_or_defclass_name)) {
			case T_STRING:
			case T_SYMBOL:
				if (Qnil == (rbDefclass = clips_environment_find_defclass(rbEnvironment, rbDefclass_or_defclass_name))) {
					rb_warn("could not find defclass");
					return Qnil;
				}
				TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);
				break;
			case T_DATA:
				TypedData_Get_Struct(rbDefclass_or_defclass_name, Defclass, &Defclass_type, defclass);
				break;
			default:
				rb_warn("defclass name must be a symbol or string");
				return Qnil;
		}
		for (
			theInstance = GetNextInstanceInClass(defclass,NULL);
			theInstance != NULL;
			theInstance = GetNextInstanceInClass(defclass,theInstance)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	} else if (include_subclasses == Qtrue) {
		switch (TYPE(rbDefclass_or_defclass_name)) {
			case T_STRING:
			case T_SYMBOL:
				if (Qnil == (rbDefclass = clips_environment_find_defclass(rbEnvironment, rbDefclass_or_defclass_name))) {
					rb_warn("could not find defclass");
					return Qnil;
				}
				TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);
				break;
			case T_DATA:
				TypedData_Get_Struct(rbDefclass_or_defclass_name, Defclass, &Defclass_type, defclass);
				break;
			default:
				rb_warn("defclass name must be a symbol or string");
				return Qnil;
		}
		for (
			theInstance = GetNextInstanceInClassAndSubclasses(&defclass,NULL,&iterator);
			theInstance != NULL;
			theInstance = GetNextInstanceInClassAndSubclasses(&defclass,theInstance,&iterator)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	// unexpected input of some kind...
	} else {
		rb_warn("unexpected arguments sent to get_instance_list!");
		return Qnil;
	}

	return returnArray;
}

static VALUE clips_environment_static_get_instance_list(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, rbDefclass_or_defclass_name, rbDefclass, include_subclasses, rbInstance, returnArray;
	Environment *env;
	Defclass *defclass;
	UDFValue iterator;
	Instance *theInstance;

	returnArray = rb_ary_new2(0);

	rb_scan_args(argc, argv, "12", &rbEnvironment, &rbDefclass_or_defclass_name, &include_subclasses);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	// defclass is undefined, ignore include_subclasses
	if (rbDefclass_or_defclass_name == Qnil) {
		for (
			theInstance = GetNextInstance(env,NULL);
			theInstance != NULL;
			theInstance = GetNextInstance(env,theInstance)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	} else if (include_subclasses != Qtrue) {
		switch (TYPE(rbDefclass_or_defclass_name)) {
			case T_STRING:
			case T_SYMBOL:
				if (Qnil == (rbDefclass = clips_environment_find_defclass(rbEnvironment, rbDefclass_or_defclass_name))) {
					rb_warn("could not find defclass");
					return Qnil;
				}
				TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);
				break;
			case T_DATA:
				TypedData_Get_Struct(rbDefclass_or_defclass_name, Defclass, &Defclass_type, defclass);
				break;
			default:
				rb_warn("defclass name must be a symbol or string");
				return Qnil;
		}
		for (
			theInstance = GetNextInstanceInClass(defclass,NULL);
			theInstance != NULL;
			theInstance = GetNextInstanceInClass(defclass,theInstance)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	} else if (include_subclasses == Qtrue) {
		switch (TYPE(rbDefclass_or_defclass_name)) {
			case T_STRING:
			case T_SYMBOL:
				if (Qnil == (rbDefclass = clips_environment_find_defclass(rbEnvironment, rbDefclass_or_defclass_name))) {
					rb_warn("could not find defclass");
					return Qnil;
				}
				TypedData_Get_Struct(rbDefclass, Defclass, &Defclass_type, defclass);
				break;
			case T_DATA:
				TypedData_Get_Struct(rbDefclass_or_defclass_name, Defclass, &Defclass_type, defclass);
				break;
			default:
				rb_warn("defclass name must be a symbol or string");
				return Qnil;
		}
		for (
			theInstance = GetNextInstanceInClassAndSubclasses(&defclass,NULL,&iterator);
			theInstance != NULL;
			theInstance = GetNextInstanceInClassAndSubclasses(&defclass,theInstance,&iterator)
		) {
			rbInstance =
				TypedData_Wrap_Struct(rb_const_get(CLASS_OF(rbEnvironment), rb_intern("Instance")), &Instance_type, theInstance);

			rb_iv_set(rbInstance, "@environment", rbEnvironment);
			rb_ary_push(returnArray, rbInstance);
		}
	// unexpected input of some kind...
	} else {
		rb_warn("unexpected arguments sent to get_instance_list!");
		return Qnil;
	}

	return returnArray;
}

static VALUE clips_environment_get_deftemplate_list(int argc, VALUE *argv, VALUE rbEnvironment) {
	VALUE defmodule_or_defmodule_name;
	Environment *env;
	Defmodule *module;
	CLIPSValue value;
	VALUE out;

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	rb_scan_args(argc, argv, "01", &defmodule_or_defmodule_name);
	switch (TYPE(defmodule_or_defmodule_name)) {
		case T_NIL:
			module = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			TypedData_Get_Struct(
				clips_environment_find_defmodule(rbEnvironment, defmodule_or_defmodule_name),
				Defmodule, &Defmodule_type, module);
			break;
		case T_DATA:
			TypedData_Get_Struct(defmodule_or_defmodule_name, Defmodule, &Defmodule_type, module);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }
	GetDeftemplateList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_static_get_deftemplate_list(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, defmodule_or_defmodule_name;
	Environment *env;
	Defmodule *module;
	CLIPSValue value;
	VALUE out;

	rb_scan_args(argc, argv, "11", &rbEnvironment, &defmodule_or_defmodule_name);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	switch (TYPE(defmodule_or_defmodule_name)) {
		case T_NIL:
			module = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			TypedData_Get_Struct(
				clips_environment_find_defmodule(rbEnvironment, defmodule_or_defmodule_name),
				Defmodule, &Defmodule_type, module);
			break;
		case T_DATA:
			TypedData_Get_Struct(defmodule_or_defmodule_name, Defmodule, &Defmodule_type, module);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }
	GetDeftemplateList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_get_defclass_list(int argc, VALUE *argv, VALUE rbEnvironment) {
	VALUE defmodule_or_defmodule_name;
	Environment *env;
	Defmodule *module;
	CLIPSValue value;
	VALUE out;

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	rb_scan_args(argc, argv, "01", &defmodule_or_defmodule_name);
	switch (TYPE(defmodule_or_defmodule_name)) {
		case T_NIL:
			module = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			TypedData_Get_Struct(
				clips_environment_find_defmodule(rbEnvironment, defmodule_or_defmodule_name),
				Defmodule, &Defmodule_type, module);
			break;
		case T_DATA:
			TypedData_Get_Struct(defmodule_or_defmodule_name, Defmodule, &Defmodule_type, module);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }
	GetDefclassList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_static_get_defclass_list(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, defmodule_or_defmodule_name;
	Environment *env;
	Defmodule *module;
	CLIPSValue value;
	VALUE out;

	rb_scan_args(argc, argv, "11", &rbEnvironment, &defmodule_or_defmodule_name);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	switch (TYPE(defmodule_or_defmodule_name)) {
		case T_NIL:
			module = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			TypedData_Get_Struct(
				clips_environment_find_defmodule(rbEnvironment, defmodule_or_defmodule_name),
				Defmodule, &Defmodule_type, module);
			break;
		case T_DATA:
			TypedData_Get_Struct(defmodule_or_defmodule_name, Defmodule, &Defmodule_type, module);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }
	GetDefclassList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_get_defrule_list(int argc, VALUE *argv, VALUE rbEnvironment) {
	VALUE defmodule_or_defmodule_name;
	Environment *env;
	Defmodule *module;
	CLIPSValue value;
	VALUE out;

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	rb_scan_args(argc, argv, "01", &defmodule_or_defmodule_name);
	switch (TYPE(defmodule_or_defmodule_name)) {
		case T_NIL:
			module = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			TypedData_Get_Struct(
				clips_environment_find_defmodule(rbEnvironment, defmodule_or_defmodule_name),
				Defmodule, &Defmodule_type, module);
			break;
		case T_DATA:
			TypedData_Get_Struct(defmodule_or_defmodule_name, Defmodule, &Defmodule_type, module);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }
	GetDefruleList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_static_get_defrule_list(int argc, VALUE *argv, VALUE klass) {
	VALUE rbEnvironment, defmodule_or_defmodule_name;
	Environment *env;
	Defmodule *module;
	CLIPSValue value;
	VALUE out;

	rb_scan_args(argc, argv, "11", &rbEnvironment, &defmodule_or_defmodule_name);

	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);
	switch (TYPE(defmodule_or_defmodule_name)) {
		case T_NIL:
			module = NULL;
			break;
		case T_STRING:
		case T_SYMBOL:
			TypedData_Get_Struct(
				clips_environment_find_defmodule(rbEnvironment, defmodule_or_defmodule_name),
				Defmodule, &Defmodule_type, module);
			break;
		case T_DATA:
			TypedData_Get_Struct(defmodule_or_defmodule_name, Defmodule, &Defmodule_type, module);
			break;
                default:
			rb_warn("defmodule name must be a symbol or string");
			return Qnil;
        }
	GetDefruleList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_defmodule_name(VALUE self)
{
	Defmodule *defmodule;

	TypedData_Get_Struct(self, Defmodule, &Defmodule_type, defmodule);

	return ID2SYM(rb_intern(DefmoduleName(defmodule)));
}

static VALUE clips_environment_defmodule_static_name(VALUE self, VALUE rbDefmodule)
{
	return clips_environment_defmodule_name(rbDefmodule);
}

static VALUE clips_environment_defmodule_pp_form(VALUE self)
{
	Defmodule *defmodule;

	TypedData_Get_Struct(self, Defmodule, &Defmodule_type, defmodule);

	return rb_str_new2(DefmodulePPForm(defmodule));
}

static VALUE clips_environment_defmodule_static_pp_form(VALUE self, VALUE rbDefmodule)
{
	return clips_environment_defmodule_pp_form(rbDefmodule);
}

static VALUE clips_environment_defmodule_set_current(VALUE self)
{
	Defmodule *module;
	Environment *env;

	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Defmodule, &Defmodule_type, module);
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	SetCurrentModule(env, module);

	return self;
}

static VALUE clips_environment_defmodule_static_set_current(VALUE self, VALUE rbDefmodule)
{
	return clips_environment_defmodule_set_current(rbDefmodule);
}

static VALUE clips_environment_defmodule_get_fact_list(VALUE self)
{
	Defmodule *module;
	Environment *env;
	CLIPSValue value;
	VALUE out;

	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Defmodule, &Defmodule_type, module);
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	GetFactList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_defmodule_static_get_fact_list(VALUE self, VALUE rbDefmodule)
{
	return clips_environment_defmodule_get_fact_list(rbDefmodule);
}

static VALUE clips_environment_defmodule_get_defclass_list(VALUE self)
{
	Defmodule *module;
	Environment *env;
	CLIPSValue value;
	VALUE out;

	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Defmodule, &Defmodule_type, module);
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	GetDefclassList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_defmodule_static_get_defclass_list(VALUE self, VALUE rbDefmodule)
{
	return clips_environment_defmodule_get_defclass_list(rbDefmodule);
}

static VALUE clips_environment_defmodule_get_deftemplate_list(VALUE self)
{
	Defmodule *module;
	Environment *env;
	CLIPSValue value;
	VALUE out;

	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Defmodule, &Defmodule_type, module);
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	GetDeftemplateList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_defmodule_static_get_deftemplate_list(VALUE self, VALUE rbDefmodule)
{
	return clips_environment_defmodule_get_deftemplate_list(rbDefmodule);
}

static VALUE clips_environment_defmodule_get_defrule_list(VALUE self)
{
	Defmodule *module;
	Environment *env;
	CLIPSValue value;
	VALUE out;

	VALUE rbEnvironment = rb_iv_get(self, "@environment");

	TypedData_Get_Struct(self, Defmodule, &Defmodule_type, module);
	TypedData_Get_Struct(rbEnvironment, Environment, &Environment_type, env);

	GetDefruleList(env, &value, module);

	CLIPSValue_to_VALUE(&value, &out, &rbEnvironment);

	return out;
}

static VALUE clips_environment_defmodule_static_get_defrule_list(VALUE self, VALUE rbDefmodule)
{
	return clips_environment_defmodule_get_defrule_list(rbDefmodule);
}

static VALUE clips_environment_get_defmodule_list(VALUE self)
{
	Environment *env;
	CLIPSValue value;
	VALUE out;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	GetDefmoduleList(env, &value);

	CLIPSValue_to_VALUE(&value, &out, &self);

	return out;
}

static VALUE clips_environment_static_get_defmodule_list(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_get_defmodule_list(rbEnvironment);
}

static VALUE clips_environment_defrule_name(VALUE self)
{
	Defrule *defrule;

	TypedData_Get_Struct(self, Defrule, &Defrule_type, defrule);

	return ID2SYM(rb_intern(DefruleName(defrule)));
}

static VALUE clips_environment_defrule_static_name(VALUE self, VALUE rbDefrule)
{
	return clips_environment_defrule_name(rbDefrule);
}

static VALUE clips_environment_defrule_pp_form(VALUE self)
{
	Defrule *defrule;
	const char *pp_form;

	TypedData_Get_Struct(self, Defrule, &Defrule_type, defrule);

	pp_form = DefrulePPForm(defrule);

	if (pp_form == NULL) {
		rb_warn("defrule did not have pp_form. This can happen if the defrule was loaded from a binary file.");
		return Qnil;
	} else {
		return rb_str_new2(pp_form);
	}
}

static VALUE clips_environment_defrule_static_pp_form(VALUE self, VALUE rbDefrule)
{
	return clips_environment_defrule_pp_form(rbDefrule);
}

static VALUE clips_environment_defrule_is_deletable(VALUE self)
{
	Defrule *defrule;

	TypedData_Get_Struct(self, Defrule, &Defrule_type, defrule);

	if (DefruleIsDeletable(defrule)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_defrule_static_is_deletable(VALUE self, VALUE rbDefrule)
{
	return clips_environment_defrule_is_deletable(rbDefrule);
}

static VALUE clips_environment_deftemplate_is_deletable(VALUE self)
{
	Deftemplate *deftemplate;

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, deftemplate);

	if (DeftemplateIsDeletable(deftemplate)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_deftemplate_static_is_deletable(VALUE self, VALUE rbDeftemplate)
{
	return clips_environment_deftemplate_is_deletable(rbDeftemplate);
}

static VALUE clips_environment_deftemplate_is_implied(VALUE self)
{
	Deftemplate *deftemplate;

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, deftemplate);

	if (deftemplate->implied) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_deftemplate_static_is_implied(VALUE self, VALUE rbDeftemplate)
{
	return clips_environment_deftemplate_is_implied(rbDeftemplate);
}

static VALUE clips_environment_deftemplate_slot_existp(VALUE self, VALUE slot_name)
{
	Deftemplate *deftemplate;
	const char *cslot_name;
	switch(TYPE(slot_name))
	{
		case T_SYMBOL:
			cslot_name = rb_id2name(SYM2ID(slot_name));
			break;
		case T_STRING:
			cslot_name = StringValueCStr(slot_name);
			break;
		default:
			rb_raise(rb_eTypeError, "Slot name must be a String or a Symbol");
			return ST_CONTINUE;
	}

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, deftemplate);

	if (DeftemplateSlotExistP(deftemplate, cslot_name)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_deftemplate_static_slot_existp(VALUE self, VALUE rbDeftemplate, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_existp(rbDeftemplate, slot_name);
}

static VALUE clips_environment_deftemplate_slot_singlep(VALUE self, VALUE slot_name)
{
	Deftemplate *deftemplate;
	const char *cslot_name;
	switch(TYPE(slot_name))
	{
		case T_SYMBOL:
			cslot_name = rb_id2name(SYM2ID(slot_name));
			break;
		case T_STRING:
			cslot_name = StringValueCStr(slot_name);
			break;
		default:
			rb_raise(rb_eTypeError, "Slot name must be a String or a Symbol");
			return ST_CONTINUE;
	}

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, deftemplate);

	if (DeftemplateSlotSingleP(deftemplate, cslot_name)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_deftemplate_static_slot_singlep(VALUE self, VALUE rbDeftemplate, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_singlep(rbDeftemplate, slot_name);
}

static VALUE clips_environment_deftemplate_slot_multip(VALUE self, VALUE slot_name)
{
	Deftemplate *deftemplate;
	const char *cslot_name;
	switch(TYPE(slot_name))
	{
		case T_SYMBOL:
			cslot_name = rb_id2name(SYM2ID(slot_name));
			break;
		case T_STRING:
			cslot_name = StringValueCStr(slot_name);
			break;
		default:
			rb_raise(rb_eTypeError, "Slot name must be a String or a Symbol");
			return ST_CONTINUE;
	}

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, deftemplate);

	if (DeftemplateSlotMultiP(deftemplate, cslot_name)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_deftemplate_static_slot_multip(VALUE self, VALUE rbDeftemplate, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_multip(rbDeftemplate, slot_name);
}

static VALUE clips_environment_deftemplate_slot_defaultp(VALUE self, VALUE slot_name)
{
	Deftemplate *deftemplate;
	const char *cslot_name;
	switch(TYPE(slot_name))
	{
		case T_SYMBOL:
			cslot_name = rb_id2name(SYM2ID(slot_name));
			break;
		case T_STRING:
			cslot_name = StringValueCStr(slot_name);
			break;
		default:
			rb_warn("Slot name must be a String or a Symbol");
			return Qnil;
	}

	TypedData_Get_Struct(self, Deftemplate, &Deftemplate_type, deftemplate);

	switch (DeftemplateSlotDefaultP(deftemplate, cslot_name)) {
		case NO_DEFAULT:
			return ID2SYM(rb_intern("NO_DEFAULT"));
		case STATIC_DEFAULT:
			return ID2SYM(rb_intern("STATIC_DEFAULT"));
		case DYNAMIC_DEFAULT:
			return ID2SYM(rb_intern("DYNAMIC_DEFAULT"));
		default:
			rb_warn("CLIPS returned something we didn't expect for slot_defaultp...");
			return Qnil;
	}
}

static VALUE clips_environment_deftemplate_static_slot_defaultp(VALUE self, VALUE rbDeftemplate, VALUE slot_name)
{
	return clips_environment_deftemplate_slot_defaultp(rbDeftemplate, slot_name);
}

static VALUE clips_environment_defrule_has_breakpoint(VALUE self)
{
	Defrule *defrule;

	TypedData_Get_Struct(self, Defrule, &Defrule_type, defrule);

	if (DefruleHasBreakpoint(defrule)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_defrule_static_has_breakpoint(VALUE self, VALUE rbDefrule)
{
	return clips_environment_defrule_has_breakpoint(rbDefrule);
}

static VALUE clips_environment_defrule_set_break(VALUE self)
{
	Defrule *defrule;

	TypedData_Get_Struct(self, Defrule, &Defrule_type, defrule);

	SetBreak(defrule);

	return Qnil;
}

static VALUE clips_environment_defrule_static_set_break(VALUE self, VALUE rbDefrule)
{
	return clips_environment_defrule_set_break(rbDefrule);
}

static VALUE clips_environment_defrule_remove_break(VALUE self)
{
	Defrule *defrule;

	TypedData_Get_Struct(self, Defrule, &Defrule_type, defrule);

	if (RemoveBreak(defrule)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_defrule_static_remove_break(VALUE self, VALUE rbDefrule)
{
	return clips_environment_defrule_remove_break(rbDefrule);
}

static VALUE clips_environment_defrule_salience(VALUE self)
{
	Defrule *defrule;

	TypedData_Get_Struct(self, Defrule, &Defrule_type, defrule);

	return INT2NUM(defrule->salience);
}

static VALUE clips_environment_defrule_static_salience(VALUE self, VALUE rbDefrule)
{
	return clips_environment_defrule_salience(rbDefrule);
}

static VALUE clips_environment_watch(VALUE self, VALUE item)
{
	Environment *env;
	WatchItem watchItem;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	if (item == ID2SYM(rb_intern("all"))) {
		watchItem = ALL;
	} else if (item == ID2SYM(rb_intern("facts"))) {
		watchItem = FACTS;
	} else if (item == ID2SYM(rb_intern("instances"))) {
		watchItem = INSTANCES;
	} else if (item == ID2SYM(rb_intern("slots"))) {
		watchItem = SLOTS;
	} else if (item == ID2SYM(rb_intern("rules"))) {
		watchItem = RULES;
	} else if (item == ID2SYM(rb_intern("activations"))) {
		watchItem = ACTIVATIONS;
	} else if (item == ID2SYM(rb_intern("messages"))) {
		watchItem = MESSAGES;
	} else if (item == ID2SYM(rb_intern("message_handlers"))) {
		watchItem = MESSAGE_HANDLERS;
	} else if (item == ID2SYM(rb_intern("generic_functions"))) {
		watchItem = GENERIC_FUNCTIONS;
	} else if (item == ID2SYM(rb_intern("methods"))) {
		watchItem = METHODS;
	} else if (item == ID2SYM(rb_intern("deffunctions"))) {
		watchItem = DEFFUNCTIONS;
	} else if (item == ID2SYM(rb_intern("compilations"))) {
		watchItem = COMPILATIONS;
	} else if (item == ID2SYM(rb_intern("statistics"))) {
		watchItem = STATISTICS;
	} else if (item == ID2SYM(rb_intern("globals"))) {
		watchItem = GLOBALS;
	} else if (item == ID2SYM(rb_intern("focus"))) {
		watchItem = FOCUS;
	} else {
		rb_warn("unknown watch item");
		return Qnil;
	}

	Watch(env, watchItem);

	return Qnil;
}

static VALUE clips_environment_static_watch(VALUE self, VALUE rbEnvironment, VALUE item)
{
	return clips_environment_watch(rbEnvironment, item);
}

static VALUE clips_environment_unwatch(VALUE self, VALUE item)
{
	Environment *env;
	WatchItem watchItem;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	if (item == ID2SYM(rb_intern("all"))) {
		watchItem = ALL;
	} else if (item == ID2SYM(rb_intern("facts"))) {
		watchItem = FACTS;
	} else if (item == ID2SYM(rb_intern("instances"))) {
		watchItem = INSTANCES;
	} else if (item == ID2SYM(rb_intern("slots"))) {
		watchItem = SLOTS;
	} else if (item == ID2SYM(rb_intern("rules"))) {
		watchItem = RULES;
	} else if (item == ID2SYM(rb_intern("activations"))) {
		watchItem = ACTIVATIONS;
	} else if (item == ID2SYM(rb_intern("messages"))) {
		watchItem = MESSAGES;
	} else if (item == ID2SYM(rb_intern("message_handlers"))) {
		watchItem = MESSAGE_HANDLERS;
	} else if (item == ID2SYM(rb_intern("generic_functions"))) {
		watchItem = GENERIC_FUNCTIONS;
	} else if (item == ID2SYM(rb_intern("methods"))) {
		watchItem = METHODS;
	} else if (item == ID2SYM(rb_intern("deffunctions"))) {
		watchItem = DEFFUNCTIONS;
	} else if (item == ID2SYM(rb_intern("compilations"))) {
		watchItem = COMPILATIONS;
	} else if (item == ID2SYM(rb_intern("statistics"))) {
		watchItem = STATISTICS;
	} else if (item == ID2SYM(rb_intern("globals"))) {
		watchItem = GLOBALS;
	} else if (item == ID2SYM(rb_intern("focus"))) {
		watchItem = FOCUS;
	} else {
		rb_warn("unknown watch item");
		return Qnil;
	}

	Unwatch(env, watchItem);

	return Qnil;
}

static VALUE clips_environment_static_unwatch(VALUE self, VALUE rbEnvironment, VALUE item)
{
	return clips_environment_unwatch(rbEnvironment, item);
}

static VALUE clips_environment_watch_all(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, ALL);

	return Qnil;
}

static VALUE clips_environment_static_watch_all(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_all(rbEnvironment);
}

static VALUE clips_environment_watch_facts(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, FACTS);

	return Qnil;
}

static VALUE clips_environment_static_watch_facts(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_facts(rbEnvironment);
}

static VALUE clips_environment_watch_instances(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, INSTANCES);

	return Qnil;
}

static VALUE clips_environment_static_watch_instances(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_instances(rbEnvironment);
}

static VALUE clips_environment_watch_slots(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, SLOTS);

	return Qnil;
}

static VALUE clips_environment_static_watch_slots(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_slots(rbEnvironment);
}

static VALUE clips_environment_watch_rules(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, RULES);

	return Qnil;
}

static VALUE clips_environment_static_watch_rules(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_rules(rbEnvironment);
}

static VALUE clips_environment_watch_activations(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, ACTIVATIONS);

	return Qnil;
}

static VALUE clips_environment_static_watch_activations(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_activations(rbEnvironment);
}

static VALUE clips_environment_watch_messages(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, MESSAGES);

	return Qnil;
}

static VALUE clips_environment_static_watch_messages(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_messages(rbEnvironment);
}

static VALUE clips_environment_watch_message_handlers(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, MESSAGE_HANDLERS);

	return Qnil;
}

static VALUE clips_environment_static_watch_message_handlers(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_message_handlers(rbEnvironment);
}

static VALUE clips_environment_watch_generic_functions(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, GENERIC_FUNCTIONS);

	return Qnil;
}

static VALUE clips_environment_static_watch_generic_functions(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_generic_functions(rbEnvironment);
}

static VALUE clips_environment_watch_methods(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, METHODS);

	return Qnil;
}

static VALUE clips_environment_static_watch_methods(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_methods(rbEnvironment);
}

static VALUE clips_environment_watch_deffunctions(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, DEFFUNCTIONS);

	return Qnil;
}

static VALUE clips_environment_static_watch_deffunctions(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_deffunctions(rbEnvironment);
}

static VALUE clips_environment_watch_compilations(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, COMPILATIONS);

	return Qnil;
}

static VALUE clips_environment_static_watch_compilations(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_compilations(rbEnvironment);
}

static VALUE clips_environment_watch_statistics(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, STATISTICS);

	return Qnil;
}

static VALUE clips_environment_static_watch_statistics(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_statistics(rbEnvironment);
}

static VALUE clips_environment_watch_globals(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, GLOBALS);

	return Qnil;
}

static VALUE clips_environment_static_watch_globals(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_globals(rbEnvironment);
}

static VALUE clips_environment_watch_focus(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Watch(env, FOCUS);

	return Qnil;
}

static VALUE clips_environment_static_watch_focus(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_watch_focus(rbEnvironment);
}

static VALUE clips_environment_unwatch_all(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, ALL);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_all(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_all(rbEnvironment);
}

static VALUE clips_environment_unwatch_facts(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, FACTS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_facts(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_facts(rbEnvironment);
}

static VALUE clips_environment_unwatch_instances(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, INSTANCES);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_instances(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_instances(rbEnvironment);
}

static VALUE clips_environment_unwatch_slots(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, SLOTS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_slots(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_slots(rbEnvironment);
}

static VALUE clips_environment_unwatch_rules(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, RULES);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_rules(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_rules(rbEnvironment);
}

static VALUE clips_environment_unwatch_activations(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, ACTIVATIONS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_activations(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_activations(rbEnvironment);
}

static VALUE clips_environment_unwatch_messages(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, MESSAGES);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_messages(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_messages(rbEnvironment);
}

static VALUE clips_environment_unwatch_message_handlers(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, MESSAGE_HANDLERS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_message_handlers(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_message_handlers(rbEnvironment);
}

static VALUE clips_environment_unwatch_generic_functions(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, GENERIC_FUNCTIONS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_generic_functions(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_generic_functions(rbEnvironment);
}

static VALUE clips_environment_unwatch_methods(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, METHODS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_methods(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_methods(rbEnvironment);
}

static VALUE clips_environment_unwatch_deffunctions(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, DEFFUNCTIONS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_deffunctions(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_deffunctions(rbEnvironment);
}

static VALUE clips_environment_unwatch_compilations(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, COMPILATIONS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_compilations(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_compilations(rbEnvironment);
}

static VALUE clips_environment_unwatch_statistics(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, STATISTICS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_statistics(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_statistics(rbEnvironment);
}

static VALUE clips_environment_unwatch_globals(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, GLOBALS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_globals(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_globals(rbEnvironment);
}

static VALUE clips_environment_unwatch_focus(VALUE self)
{
	Environment *env;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	Unwatch(env, FOCUS);

	return Qnil;
}

static VALUE clips_environment_static_unwatch_focus(VALUE self, VALUE rbEnvironment)
{
	return clips_environment_unwatch_focus(rbEnvironment);
}

static VALUE clips_environment_get_watch_state(VALUE self, VALUE item)
{
	Environment *env;
	WatchItem watchItem;

	TypedData_Get_Struct(self, Environment, &Environment_type, env);

	if (item == ID2SYM(rb_intern("all"))) {
		watchItem = ALL;
	} else if (item == ID2SYM(rb_intern("facts"))) {
		watchItem = FACTS;
	} else if (item == ID2SYM(rb_intern("instances"))) {
		watchItem = INSTANCES;
	} else if (item == ID2SYM(rb_intern("slots"))) {
		watchItem = SLOTS;
	} else if (item == ID2SYM(rb_intern("rules"))) {
		watchItem = RULES;
	} else if (item == ID2SYM(rb_intern("activations"))) {
		watchItem = ACTIVATIONS;
	} else if (item == ID2SYM(rb_intern("messages"))) {
		watchItem = MESSAGES;
	} else if (item == ID2SYM(rb_intern("message_handlers"))) {
		watchItem = MESSAGE_HANDLERS;
	} else if (item == ID2SYM(rb_intern("generic_functions"))) {
		watchItem = GENERIC_FUNCTIONS;
	} else if (item == ID2SYM(rb_intern("methods"))) {
		watchItem = METHODS;
	} else if (item == ID2SYM(rb_intern("deffunctions"))) {
		watchItem = DEFFUNCTIONS;
	} else if (item == ID2SYM(rb_intern("compilations"))) {
		watchItem = COMPILATIONS;
	} else if (item == ID2SYM(rb_intern("statistics"))) {
		watchItem = STATISTICS;
	} else if (item == ID2SYM(rb_intern("globals"))) {
		watchItem = GLOBALS;
	} else if (item == ID2SYM(rb_intern("focus"))) {
		watchItem = FOCUS;
	} else {
		rb_warn("unknown watch item");
		return Qnil;
	}

	if (GetWatchState(env, watchItem)) {
		return Qtrue;
	} else {
		return Qfalse;
	}
}

static VALUE clips_environment_static_get_watch_state(VALUE self, VALUE rbEnvironment, VALUE item)
{
	return clips_environment_get_watch_state(rbEnvironment, item);
}

void Init_clipsruby(void)
{
	VALUE rbCLIPS = rb_define_module("CLIPS");
	rb_define_module_function(rbCLIPS, "create_environment", create_environment, 0);

	VALUE rbEnvironment = rb_define_class_under(rbCLIPS, "Environment", rb_cObject);
	rb_define_alloc_func(rbEnvironment, environment_alloc);
	rb_define_singleton_method(rbEnvironment, "assert_string", clips_environment_static_assert_string, 2);
	rb_define_method(rbEnvironment, "assert_string", clips_environment_assert_string, 1);
	rb_define_singleton_method(rbEnvironment, "facts", clips_environment_static_facts, 1);
	rb_define_method(rbEnvironment, "facts", clips_environment_facts, 0);
	rb_define_singleton_method(rbEnvironment, "assert_hash", clips_environment_static_assert_hash, 3);
	rb_define_method(rbEnvironment, "assert_hash", clips_environment_assert_hash, 2);
	rb_define_singleton_method(rbEnvironment, "build", clips_environment_static_build, 2);
	rb_define_method(rbEnvironment, "build", clips_environment_build, 1);
	rb_define_singleton_method(rbEnvironment, "add_udf", clips_environment_static_add_udf, -1);
	rb_define_method(rbEnvironment, "add_udf", clips_environment_add_udf, -1);
	rb_define_singleton_method(rbEnvironment, "run", clips_environment_static_run, -1);
	rb_define_method(rbEnvironment, "run", clips_environment_run, -1);
	rb_define_singleton_method(rbEnvironment, "_eval", clips_environment_static_eval, 2);
	rb_define_method(rbEnvironment, "_eval", clips_environment_eval, 1);
	rb_define_singleton_method(rbEnvironment, "find_fact", clips_environment_static_find_fact, -1);
	rb_define_method(rbEnvironment, "find_fact", clips_environment_find_fact, -1);
	rb_define_singleton_method(rbEnvironment, "find_all_facts", clips_environment_static_find_all_facts, -1);
	rb_define_method(rbEnvironment, "find_all_facts", clips_environment_find_all_facts, -1);
	rb_define_singleton_method(rbEnvironment, "batch_star", clips_environment_static_batch_star, 2);
	rb_define_method(rbEnvironment, "batch_star", clips_environment_batch_star, 1);
	rb_define_singleton_method(rbEnvironment, "clear", clips_environment_static_clear, 1);
	rb_define_method(rbEnvironment, "clear", clips_environment_clear, 0);
	rb_define_singleton_method(rbEnvironment, "reset", clips_environment_static_reset, 1);
	rb_define_method(rbEnvironment, "reset", clips_environment_reset, 0);
	rb_define_singleton_method(rbEnvironment, "save", clips_environment_static_save, 2);
	rb_define_method(rbEnvironment, "save", clips_environment_save, 1);
	rb_define_singleton_method(rbEnvironment, "load", clips_environment_static_load, 2);
	rb_define_method(rbEnvironment, "load", clips_environment_load, 1);
	rb_define_singleton_method(rbEnvironment, "save_facts", clips_environment_static_save_facts, -1);
	rb_define_method(rbEnvironment, "save_facts", clips_environment_save_facts, -1);
	rb_define_singleton_method(rbEnvironment, "load_facts", clips_environment_static_load_facts, 2);
	rb_define_method(rbEnvironment, "load_facts", clips_environment_load_facts, 1);
	rb_define_singleton_method(rbEnvironment, "bsave", clips_environment_static_bsave, 2);
	rb_define_method(rbEnvironment, "bsave", clips_environment_bsave, 1);
	rb_define_singleton_method(rbEnvironment, "bload", clips_environment_static_bload, 2);
	rb_define_method(rbEnvironment, "bload", clips_environment_bload, 1);
	rb_define_singleton_method(rbEnvironment, "bsave_facts", clips_environment_static_bsave_facts, -1);
	rb_define_method(rbEnvironment, "bsave_facts", clips_environment_bsave_facts, -1);
	rb_define_singleton_method(rbEnvironment, "bload_facts", clips_environment_static_bload_facts, 2);
	rb_define_method(rbEnvironment, "bload_facts", clips_environment_bload_facts, 1);
	rb_define_singleton_method(rbEnvironment, "find_defrule", clips_environment_static_find_defrule, 2);
	rb_define_method(rbEnvironment, "find_defrule", clips_environment_find_defrule, 1);
	rb_define_singleton_method(rbEnvironment, "find_defmodule", clips_environment_static_find_defmodule, 2);
	rb_define_method(rbEnvironment, "find_defmodule", clips_environment_find_defmodule, 1);
	rb_define_singleton_method(rbEnvironment, "find_deftemplate", clips_environment_static_find_deftemplate, 2);
	rb_define_method(rbEnvironment, "find_deftemplate", clips_environment_find_deftemplate, 1);
	rb_define_singleton_method(rbEnvironment, "find_defclass", clips_environment_static_find_defclass, 2);
	rb_define_method(rbEnvironment, "find_defclass", clips_environment_find_defclass, 1);
	rb_define_singleton_method(rbEnvironment, "find_instance", clips_environment_static_find_instance, -1);
	rb_define_method(rbEnvironment, "find_instance", clips_environment_find_instance, -1);
	rb_define_singleton_method(rbEnvironment, "get_current_module", clips_environment_static_get_current_module, 1);
	rb_define_method(rbEnvironment, "get_current_module", clips_environment_get_current_module, 0);
	rb_define_singleton_method(rbEnvironment, "set_current_module", clips_environment_static_set_current_module, 2);
	rb_define_method(rbEnvironment, "set_current_module", clips_environment_set_current_module, 1);
	rb_define_singleton_method(rbEnvironment, "get_fact_list", clips_environment_static_get_fact_list, -1);
	rb_define_method(rbEnvironment, "get_fact_list", clips_environment_get_fact_list, -1);
	rb_define_singleton_method(rbEnvironment, "get_instance_list", clips_environment_static_get_instance_list, -1);
	rb_define_method(rbEnvironment, "get_instance_list", clips_environment_get_instance_list, -1);
	rb_define_singleton_method(rbEnvironment, "get_deftemplate_list", clips_environment_static_get_deftemplate_list, -1);
	rb_define_method(rbEnvironment, "get_deftemplate_list", clips_environment_get_deftemplate_list, -1);
	rb_define_singleton_method(rbEnvironment, "get_defclass_list", clips_environment_static_get_defclass_list, -1);
	rb_define_method(rbEnvironment, "get_defclass_list", clips_environment_get_defclass_list, -1);
	rb_define_singleton_method(rbEnvironment, "get_defrule_list", clips_environment_static_get_defrule_list, -1);
	rb_define_method(rbEnvironment, "get_defrule_list", clips_environment_get_defrule_list, -1);
	rb_define_singleton_method(rbEnvironment, "get_defmodule_list", clips_environment_static_get_defmodule_list, 1);
	rb_define_method(rbEnvironment, "get_defmodule_list", clips_environment_get_defmodule_list, 0);
	rb_define_singleton_method(rbEnvironment, "find_deffacts", clips_environment_static_find_deffacts, 2);
	rb_define_method(rbEnvironment, "find_deffacts", clips_environment_find_deffacts, 1);
	rb_define_singleton_method(rbEnvironment, "watch", clips_environment_static_watch, 2);
	rb_define_method(rbEnvironment, "watch", clips_environment_watch, 1);
	rb_define_singleton_method(rbEnvironment, "watch_all", clips_environment_static_watch_all, 1);
	rb_define_method(rbEnvironment, "watch_all", clips_environment_watch_all, 0);
	rb_define_singleton_method(rbEnvironment, "watch_facts", clips_environment_static_watch_facts, 1);
	rb_define_method(rbEnvironment, "watch_facts", clips_environment_watch_facts, 0);
	rb_define_singleton_method(rbEnvironment, "watch_instances", clips_environment_static_watch_instances, 1);
	rb_define_method(rbEnvironment, "watch_instances", clips_environment_watch_instances, 0);
	rb_define_singleton_method(rbEnvironment, "watch_slots", clips_environment_static_watch_slots, 1);
	rb_define_method(rbEnvironment, "watch_slots", clips_environment_watch_slots, 0);
	rb_define_singleton_method(rbEnvironment, "watch_rules", clips_environment_static_watch_rules, 1);
	rb_define_method(rbEnvironment, "watch_rules", clips_environment_watch_rules, 0);
	rb_define_singleton_method(rbEnvironment, "watch_activations", clips_environment_static_watch_activations, 1);
	rb_define_method(rbEnvironment, "watch_activations", clips_environment_watch_activations, 0);
	rb_define_singleton_method(rbEnvironment, "watch_messages", clips_environment_static_watch_messages, 1);
	rb_define_method(rbEnvironment, "watch_messages", clips_environment_watch_messages, 0);
	rb_define_singleton_method(rbEnvironment, "watch_message_handlers", clips_environment_static_watch_message_handlers, 1);
	rb_define_method(rbEnvironment, "watch_message_handlers", clips_environment_watch_message_handlers, 0);
	rb_define_singleton_method(rbEnvironment, "watch_generic_functions", clips_environment_static_watch_generic_functions, 1);
	rb_define_method(rbEnvironment, "watch_generic_functions", clips_environment_watch_generic_functions, 0);
	rb_define_singleton_method(rbEnvironment, "watch_methods", clips_environment_static_watch_methods, 1);
	rb_define_method(rbEnvironment, "watch_methods", clips_environment_watch_methods, 0);
	rb_define_singleton_method(rbEnvironment, "watch_deffunctions", clips_environment_static_watch_deffunctions, 1);
	rb_define_method(rbEnvironment, "watch_deffunctions", clips_environment_watch_deffunctions, 0);
	rb_define_singleton_method(rbEnvironment, "watch_compilations", clips_environment_static_watch_compilations, 1);
	rb_define_method(rbEnvironment, "watch_compilations", clips_environment_watch_compilations, 0);
	rb_define_singleton_method(rbEnvironment, "watch_statistics", clips_environment_static_watch_statistics, 1);
	rb_define_method(rbEnvironment, "watch_statistics", clips_environment_watch_statistics, 0);
	rb_define_singleton_method(rbEnvironment, "watch_globals", clips_environment_static_watch_globals, 1);
	rb_define_method(rbEnvironment, "watch_globals", clips_environment_watch_globals, 0);
	rb_define_singleton_method(rbEnvironment, "watch_focus", clips_environment_static_watch_focus, 1);
	rb_define_method(rbEnvironment, "watch_focus", clips_environment_watch_focus, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch", clips_environment_static_unwatch, 2);
	rb_define_method(rbEnvironment, "unwatch", clips_environment_unwatch, 1);
	rb_define_singleton_method(rbEnvironment, "unwatch_all", clips_environment_static_unwatch_all, 1);
	rb_define_method(rbEnvironment, "unwatch_all", clips_environment_unwatch_all, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_facts", clips_environment_static_unwatch_facts, 1);
	rb_define_method(rbEnvironment, "unwatch_facts", clips_environment_unwatch_facts, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_instances", clips_environment_static_unwatch_instances, 1);
	rb_define_method(rbEnvironment, "unwatch_instances", clips_environment_unwatch_instances, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_slots", clips_environment_static_unwatch_slots, 1);
	rb_define_method(rbEnvironment, "unwatch_slots", clips_environment_unwatch_slots, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_rules", clips_environment_static_unwatch_rules, 1);
	rb_define_method(rbEnvironment, "unwatch_rules", clips_environment_unwatch_rules, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_activations", clips_environment_static_unwatch_activations, 1);
	rb_define_method(rbEnvironment, "unwatch_activations", clips_environment_unwatch_activations, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_messages", clips_environment_static_unwatch_messages, 1);
	rb_define_method(rbEnvironment, "unwatch_messages", clips_environment_unwatch_messages, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_message_handlers", clips_environment_static_unwatch_message_handlers, 1);
	rb_define_method(rbEnvironment, "unwatch_message_handlers", clips_environment_unwatch_message_handlers, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_generic_functions", clips_environment_static_unwatch_generic_functions, 1);
	rb_define_method(rbEnvironment, "unwatch_generic_functions", clips_environment_unwatch_generic_functions, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_methods", clips_environment_static_unwatch_methods, 1);
	rb_define_method(rbEnvironment, "unwatch_methods", clips_environment_unwatch_methods, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_deffunctions", clips_environment_static_unwatch_deffunctions, 1);
	rb_define_method(rbEnvironment, "unwatch_deffunctions", clips_environment_unwatch_deffunctions, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_compilations", clips_environment_static_unwatch_compilations, 1);
	rb_define_method(rbEnvironment, "unwatch_compilations", clips_environment_unwatch_compilations, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_statistics", clips_environment_static_unwatch_statistics, 1);
	rb_define_method(rbEnvironment, "unwatch_statistics", clips_environment_unwatch_statistics, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_globals", clips_environment_static_unwatch_globals, 1);
	rb_define_method(rbEnvironment, "unwatch_globals", clips_environment_unwatch_globals, 0);
	rb_define_singleton_method(rbEnvironment, "unwatch_focus", clips_environment_static_unwatch_focus, 1);
	rb_define_method(rbEnvironment, "unwatch_focus", clips_environment_unwatch_focus, 0);
	rb_define_singleton_method(rbEnvironment, "get_watch_state", clips_environment_static_get_watch_state, 2);
	rb_define_method(rbEnvironment, "get_watch_state", clips_environment_get_watch_state, 1);
	rb_define_singleton_method(rbEnvironment, "make_instance", clips_environment_static_make_instance, 2);
	rb_define_method(rbEnvironment, "make_instance", clips_environment_make_instance, 1);

	VALUE rbDeffacts = rb_define_class_under(rbEnvironment, "Deffacts", rb_cObject);
	rb_define_alloc_func(rbDeffacts, deffacts_alloc);
	rb_define_singleton_method(rbDeffacts, "name", clips_environment_deffacts_static_name, 1);
	rb_define_method(rbDeffacts, "name", clips_environment_deffacts_name, 0);
	rb_define_singleton_method(rbDeffacts, "pp_form", clips_environment_deffacts_static_pp_form, 1);
	rb_define_method(rbDeffacts, "pp_form", clips_environment_deffacts_pp_form, 0);

	VALUE rbDeftemplate = rb_define_class_under(rbEnvironment, "Deftemplate", rb_cObject);
	rb_define_alloc_func(rbDeftemplate, deftemplate_alloc);
	rb_define_singleton_method(rbDeftemplate, "name", clips_environment_deftemplate_static_name, 1);
	rb_define_method(rbDeftemplate, "name", clips_environment_deftemplate_name, 0);
	rb_define_singleton_method(rbDeftemplate, "pp_form", clips_environment_deftemplate_static_pp_form, 1);
	rb_define_method(rbDeftemplate, "pp_form", clips_environment_deftemplate_pp_form, 0);
	rb_define_singleton_method(rbDeftemplate, "assert_hash", clips_environment_deftemplate_static_assert_hash, 2);
	rb_define_method(rbDeftemplate, "assert_hash", clips_environment_deftemplate_assert_hash, 1);
	rb_define_singleton_method(rbDeftemplate, "defmodule_name", clips_environment_deftemplate_static_defmodule_name, 1);
	rb_define_method(rbDeftemplate, "defmodule_name", clips_environment_deftemplate_defmodule_name, 0);
	rb_define_singleton_method(rbDeftemplate, "slot_names", clips_environment_deftemplate_static_slot_names, 1);
	rb_define_method(rbDeftemplate, "slot_names", clips_environment_deftemplate_slot_names, 0);
	rb_define_singleton_method(rbDeftemplate, "is_deletable", clips_environment_deftemplate_static_is_deletable, 1);
	rb_define_method(rbDeftemplate, "is_deletable", clips_environment_deftemplate_is_deletable, 0);
	rb_define_singleton_method(rbDeftemplate, "slot_allowed_values", clips_environment_deftemplate_static_slot_allowed_values, 2);
	rb_define_method(rbDeftemplate, "slot_allowed_values", clips_environment_deftemplate_slot_allowed_values, 1);
	rb_define_singleton_method(rbDeftemplate, "slot_default_value", clips_environment_deftemplate_static_slot_default_value, 2);
	rb_define_method(rbDeftemplate, "slot_default_value", clips_environment_deftemplate_slot_default_value, 1);
	rb_define_singleton_method(rbDeftemplate, "slot_types", clips_environment_deftemplate_static_slot_types, 2);
	rb_define_method(rbDeftemplate, "slot_types", clips_environment_deftemplate_slot_types, 1);
	rb_define_singleton_method(rbDeftemplate, "slot_range", clips_environment_deftemplate_static_slot_range, 2);
	rb_define_method(rbDeftemplate, "slot_range", clips_environment_deftemplate_slot_range, 1);
	rb_define_singleton_method(rbDeftemplate, "slot_cardinality", clips_environment_deftemplate_static_slot_cardinality, 2);
	rb_define_method(rbDeftemplate, "slot_cardinality", clips_environment_deftemplate_slot_cardinality, 1);
	rb_define_singleton_method(rbDeftemplate, "slot_existp", clips_environment_deftemplate_static_slot_existp, 2);
	rb_define_method(rbDeftemplate, "slot_existp", clips_environment_deftemplate_slot_existp, 1);
	rb_define_singleton_method(rbDeftemplate, "slot_singlep", clips_environment_deftemplate_static_slot_singlep, 2);
	rb_define_method(rbDeftemplate, "slot_singlep", clips_environment_deftemplate_slot_singlep, 1);
	rb_define_singleton_method(rbDeftemplate, "slot_multip", clips_environment_deftemplate_static_slot_multip, 2);
	rb_define_method(rbDeftemplate, "slot_multip", clips_environment_deftemplate_slot_multip, 1);
	rb_define_singleton_method(rbDeftemplate, "slot_defaultp", clips_environment_deftemplate_static_slot_defaultp, 2);
	rb_define_method(rbDeftemplate, "slot_defaultp", clips_environment_deftemplate_slot_defaultp, 1);
	rb_define_singleton_method(rbDeftemplate, "is_implied", clips_environment_deftemplate_static_is_implied, 1);
	rb_define_method(rbDeftemplate, "is_implied", clips_environment_deftemplate_is_implied, 0);

	VALUE rbDefmodule = rb_define_class_under(rbEnvironment, "Defmodule", rb_cObject);
	rb_define_alloc_func(rbDefmodule, defmodule_alloc);
	rb_define_singleton_method(rbDefmodule, "name", clips_environment_defmodule_static_name, 1);
	rb_define_method(rbDefmodule, "name", clips_environment_defmodule_name, 0);
	rb_define_singleton_method(rbDefmodule, "pp_form", clips_environment_defmodule_static_pp_form, 1);
	rb_define_method(rbDefmodule, "pp_form", clips_environment_defmodule_pp_form, 0);
	rb_define_singleton_method(rbDefmodule, "set_current", clips_environment_defmodule_static_set_current, 1);
	rb_define_method(rbDefmodule, "set_current", clips_environment_defmodule_set_current, 0);
	rb_define_singleton_method(rbDefmodule, "get_defclass_list", clips_environment_defmodule_static_get_defclass_list, 1);
	rb_define_method(rbDefmodule, "get_defclass_list", clips_environment_defmodule_get_defclass_list, 0);
	rb_define_singleton_method(rbDefmodule, "get_fact_list", clips_environment_defmodule_static_get_fact_list, 1);
	rb_define_method(rbDefmodule, "get_fact_list", clips_environment_defmodule_get_fact_list, 0);
	rb_define_singleton_method(rbDefmodule, "get_deftemplate_list", clips_environment_defmodule_static_get_deftemplate_list, 1);
	rb_define_method(rbDefmodule, "get_deftemplate_list", clips_environment_defmodule_get_deftemplate_list, 0);
	rb_define_singleton_method(rbDefmodule, "get_defrule_list", clips_environment_defmodule_static_get_defrule_list, 1);
	rb_define_method(rbDefmodule, "get_defrule_list", clips_environment_defmodule_get_defrule_list, 0);
	rb_define_singleton_method(rbDefmodule, "find_instance", clips_environment_defmodule_static_find_instance, -1);
	rb_define_method(rbDefmodule, "find_instance", clips_environment_defmodule_find_instance, -1);

	VALUE rbFact = rb_define_class_under(rbEnvironment, "Fact", rb_cObject);
	rb_define_alloc_func(rbFact, fact_alloc);
	rb_define_singleton_method(rbFact, "deftemplate", clips_environment_fact_static_deftemplate, 1);
	rb_define_method(rbFact, "deftemplate", clips_environment_fact_deftemplate, 0);
	rb_define_singleton_method(rbFact, "deftemplate_name", clips_environment_fact_static_deftemplate_name, 1);
	rb_define_method(rbFact, "deftemplate_name", clips_environment_fact_deftemplate_name, 0);
	rb_define_singleton_method(rbFact, "to_h", clips_environment_fact_static_to_h, 1);
	rb_define_method(rbFact, "to_h", clips_environment_fact_to_h, 0);
	rb_define_singleton_method(rbFact, "slot_names", clips_environment_fact_static_slot_names, 1);
	rb_define_method(rbFact, "slot_names", clips_environment_fact_slot_names, 0);
	rb_define_singleton_method(rbFact, "get_slot", clips_environment_fact_static_get_slot, 2);
	rb_define_method(rbFact, "get_slot", clips_environment_fact_get_slot, 1);
	rb_define_singleton_method(rbFact, "retract", clips_environment_fact_static_retract, 1);
	rb_define_method(rbFact, "retract", clips_environment_fact_retract, 0);
	rb_define_singleton_method(rbFact, "modify", clips_environment_fact_static_modify, 2);
	rb_define_method(rbFact, "modify", clips_environment_fact_modify, 1);
	rb_define_singleton_method(rbFact, "index", clips_environment_fact_static_index, 1);
	rb_define_method(rbFact, "index", clips_environment_fact_index, 0);
	rb_define_singleton_method(rbFact, "pp_form", clips_environment_fact_static_pp_form, -1);
	rb_define_method(rbFact, "pp_form", clips_environment_fact_pp_form, -1);
	rb_define_singleton_method(rbFact, "existp", clips_environment_fact_static_existp, 1);
	rb_define_method(rbFact, "existp", clips_environment_fact_existp, 0);

	VALUE rbDefrule = rb_define_class_under(rbEnvironment, "Defrule", rb_cObject);
	rb_define_alloc_func(rbDefrule, defrule_alloc);
	rb_define_singleton_method(rbDefrule, "name", clips_environment_defrule_static_name, 1);
	rb_define_method(rbDefrule, "name", clips_environment_defrule_name, 0);
	rb_define_singleton_method(rbDefrule, "pp_form", clips_environment_defrule_static_pp_form, 1);
	rb_define_method(rbDefrule, "pp_form", clips_environment_defrule_pp_form, 0);
	rb_define_singleton_method(rbDefrule, "is_deletable", clips_environment_defrule_static_is_deletable, 1);
	rb_define_method(rbDefrule, "is_deletable", clips_environment_defrule_is_deletable, 0);
	rb_define_singleton_method(rbDefrule, "has_breakpoint", clips_environment_defrule_static_has_breakpoint, 1);
	rb_define_method(rbDefrule, "has_breakpoint", clips_environment_defrule_has_breakpoint, 0);
	rb_define_singleton_method(rbDefrule, "set_break", clips_environment_defrule_static_set_break, 1);
	rb_define_method(rbDefrule, "set_break", clips_environment_defrule_set_break, 0);
	rb_define_singleton_method(rbDefrule, "remove_break", clips_environment_defrule_static_remove_break, 1);
	rb_define_method(rbDefrule, "remove_break", clips_environment_defrule_remove_break, 0);
	rb_define_singleton_method(rbDefrule, "salience", clips_environment_defrule_static_salience, 1);
	rb_define_method(rbDefrule, "salience", clips_environment_defrule_salience, 0);
	rb_define_singleton_method(rbDefrule, "defmodule_name", clips_environment_defrule_static_defmodule_name, 1);
	rb_define_method(rbDefrule, "defmodule_name", clips_environment_defrule_defmodule_name, 0);

	VALUE rbDefclass = rb_define_class_under(rbEnvironment, "Defclass", rb_cObject);
	rb_define_alloc_func(rbDefclass, defclass_alloc);
	rb_define_singleton_method(rbDefclass, "name", clips_environment_defclass_static_name, 1);
	rb_define_method(rbDefclass, "name", clips_environment_defclass_name, 0);
	rb_define_singleton_method(rbDefclass, "defmodule_name", clips_environment_defclass_static_defmodule_name, 1);
	rb_define_method(rbDefclass, "defmodule_name", clips_environment_defclass_defmodule_name, 0);
	rb_define_singleton_method(rbDefclass, "pp_form", clips_environment_defclass_static_pp_form, 1);
	rb_define_method(rbDefclass, "pp_form", clips_environment_defclass_pp_form, 0);
	rb_define_singleton_method(rbDefclass, "get_instance_list", clips_environment_defclass_static_get_instance_list, -1);
	rb_define_method(rbDefclass, "get_instance_list", clips_environment_defclass_get_instance_list, -1);
	rb_define_singleton_method(rbDefclass, "superclasses", clips_environment_defclass_static_superclasses, -1);
	rb_define_method(rbDefclass, "superclasses", clips_environment_defclass_superclasses, -1);
	rb_define_singleton_method(rbDefclass, "subclasses", clips_environment_defclass_static_subclasses, -1);
	rb_define_method(rbDefclass, "subclasses", clips_environment_defclass_subclasses, -1);

	VALUE rbInstance = rb_define_class_under(rbEnvironment, "Instance", rb_cObject);
	rb_define_alloc_func(rbInstance, instance_alloc);
	rb_define_singleton_method(rbInstance, "unmake", clips_environment_instance_static_unmake, 1);
	rb_define_method(rbInstance, "unmake", clips_environment_instance_unmake, 0);
	rb_define_singleton_method(rbInstance, "delete", clips_environment_instance_static_delete, 1);
	rb_define_method(rbInstance, "delete", clips_environment_instance_delete, 0);
	rb_define_singleton_method(rbInstance, "defclass", clips_environment_instance_static_defclass, 1);
	rb_define_method(rbInstance, "defclass", clips_environment_instance_defclass, 0);
	rb_define_singleton_method(rbInstance, "name", clips_environment_instance_static_name, 1);
	rb_define_method(rbInstance, "name", clips_environment_instance_name, 0);
	rb_define_singleton_method(rbInstance, "pp_form", clips_environment_instance_static_pp_form, 1);
	rb_define_method(rbInstance, "pp_form", clips_environment_instance_pp_form, 0);
}
