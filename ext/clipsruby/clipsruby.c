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

VALUE defrule_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Defrule_type, NULL);
}

VALUE fact_alloc(VALUE self)
{
	return TypedData_Wrap_Struct(self, &Fact_type, NULL);
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

	VALUE rb_fact =
		TypedData_Wrap_Struct(rb_const_get(CLASS_OF(self), rb_intern("Fact")), &Fact_type, fact);

	rb_iv_set(rb_fact, "@environment", self);

	return rb_fact;
}

static VALUE clips_environment_static_assert_string(VALUE self, VALUE rbEnvironment, VALUE string)
{
	return clips_environment_assert_string(rbEnvironment, string);
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
		case EXTERNAL_ADDRESS_TYPE:
		case INSTANCE_ADDRESS_TYPE:
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
		case EXTERNAL_ADDRESS_TYPE:
		case INSTANCE_ADDRESS_TYPE:
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

	TypedData_Get_Struct(self, Defrule, &Defrule_type, defrule);

	return rb_str_new2(DefrulePPForm(defrule));
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
	rb_define_singleton_method(rbEnvironment, "get_current_module", clips_environment_static_get_current_module, 1);
	rb_define_method(rbEnvironment, "get_current_module", clips_environment_get_current_module, 0);
	rb_define_singleton_method(rbEnvironment, "set_current_module", clips_environment_static_set_current_module, 2);
	rb_define_method(rbEnvironment, "set_current_module", clips_environment_set_current_module, 1);
	rb_define_singleton_method(rbEnvironment, "get_fact_list", clips_environment_static_get_fact_list, -1);
	rb_define_method(rbEnvironment, "get_fact_list", clips_environment_get_fact_list, -1);
	rb_define_singleton_method(rbEnvironment, "find_deffacts", clips_environment_static_find_deffacts, 2);
	rb_define_method(rbEnvironment, "find_deffacts", clips_environment_find_deffacts, 1);

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

	VALUE rbDefmodule = rb_define_class_under(rbEnvironment, "Defmodule", rb_cObject);
	rb_define_alloc_func(rbDefmodule, defmodule_alloc);
	rb_define_singleton_method(rbDefmodule, "name", clips_environment_defmodule_static_name, 1);
	rb_define_method(rbDefmodule, "name", clips_environment_defmodule_name, 0);
	rb_define_singleton_method(rbDefmodule, "pp_form", clips_environment_defmodule_static_pp_form, 1);
	rb_define_method(rbDefmodule, "pp_form", clips_environment_defmodule_pp_form, 0);
	rb_define_singleton_method(rbDefmodule, "set_current", clips_environment_defmodule_static_set_current, 1);
	rb_define_method(rbDefmodule, "set_current", clips_environment_defmodule_set_current, 0);
	rb_define_singleton_method(rbDefmodule, "get_fact_list", clips_environment_defmodule_static_get_fact_list, 1);
	rb_define_method(rbDefmodule, "get_fact_list", clips_environment_defmodule_get_fact_list, 0);

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

	VALUE rbInstance = rb_define_class_under(rbEnvironment, "Instance", rb_cObject);
}
