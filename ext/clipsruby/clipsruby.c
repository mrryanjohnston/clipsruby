#include "clips.h"
#include "ruby.h"

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

static void CLIPSValue_to_VALUE(CLIPSValue *from, VALUE *value, Environment *env)
{
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
				CLIPSValue_to_VALUE(&from->multifieldValue->contents[i], &innerValue, env);
				rb_ary_push(*value, innerValue);
			}
			break;
		case SYMBOL_TYPE:
			if (from->lexemeValue == TrueSymbol(env)) {
				*value = Qtrue;
			} else if (from->lexemeValue == FalseSymbol(env)) {
				*value = Qfalse;
			} else {
				*value = rb_str_new2(from->lexemeValue->contents);
			}
			break;
		case INTEGER_TYPE:
			*value = LONG2NUM(from->integerValue->contents);
			break;
		case STRING_TYPE:
		case INSTANCE_NAME_TYPE:
			*value = rb_str_new2(from->lexemeValue->contents);
			break;
		case EXTERNAL_ADDRESS_TYPE:
		case FACT_ADDRESS_TYPE:
		case INSTANCE_ADDRESS_TYPE:
		default:
			WriteString(env,STDERR,"Unsupported data type returned from function\n");
			*value = Qnil;
			break;
	}
}

static void UDFValue_to_VALUE(UDFValue *from, VALUE *value, Environment *env)
{
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
				CLIPSValue_to_VALUE(&from->multifieldValue->contents[i], &innerValue, env);
				rb_ary_push(*value, innerValue);
			}
			break;
		case SYMBOL_TYPE:
			if (from->lexemeValue == TrueSymbol(env)) {
				*value = Qtrue;
			} else if (from->lexemeValue == FalseSymbol(env)) {
				*value = Qfalse;
			} else {
				*value = rb_str_new2(from->lexemeValue->contents);
			}
			break;
		case INTEGER_TYPE:
			*value = LONG2NUM(from->integerValue->contents);
			break;
		case STRING_TYPE:
		case INSTANCE_NAME_TYPE:
			*value = rb_str_new2(from->lexemeValue->contents);
			break;
		case EXTERNAL_ADDRESS_TYPE:
		case FACT_ADDRESS_TYPE:
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
	VALUE theValue;
	while (UDFHasNextArgument(context))
	{
		UDFNextArgument(context, ANY_TYPE_BITS, &theArg);
		UDFValue_to_VALUE(&theArg, current_argv, theEnv);
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

	return NUM2INT(Run(env, NUM2INT(integer)));
}

static VALUE clips_environment_static_run(int argc, VALUE *argv, VALUE klass) {
	VALUE environment, integer;
	Environment *env;

	rb_scan_args(argc, argv, "11", &environment, &integer);
	if (NIL_P(integer)) {
		integer = INT2NUM(-1);
	}

	TypedData_Get_Struct(environment, Environment, &Environment_type, env);

	return NUM2INT(Run(env, NUM2INT(integer)));
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

	VALUE rbFact = rb_define_class_under(rbEnvironment, "Fact", rb_cObject);
	rb_define_singleton_method(rbFact, "deftemplate_name", clips_environment_fact_static_deftemplate_name, 1);
	rb_define_method(rbFact, "deftemplate_name", clips_environment_fact_deftemplate_name, 0);
	/*
	rb_define_singleton_method(rbFact, "to_h", clips_environment_fact_static_to_h, 1);
	rb_define_method(rbFact, "to_h", clips_environment_fact_to_h, 0);
	*/

	VALUE rbInstance = rb_define_class_under(rbEnvironment, "Instance", rb_cObject);
}
