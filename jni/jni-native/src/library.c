#include <stdio.h>
#include "simple_Library_00024.h"

/*
 * Class:     simple_Library__
 * Method:    say
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_simple_Library_00024_say
(JNIEnv *env, jobject clazz, jstring message) {
	const char* msg = (*env)->GetStringUTFChars(env, message, 0);
	fprintf(stdout, "Printing from native library: %s\n", msg);
	fflush(stdout);
	(*env)->ReleaseStringUTFChars(env, message, msg);
	return 42;
}
