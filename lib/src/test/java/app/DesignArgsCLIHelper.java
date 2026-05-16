package app;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Reflection helper for invoking the DFHDL plugin-generated
 * {@code top_TestCLIFoo} DFApp object from the Scala test suite.
 *
 * <p>The DFHDL plugin synthesizes that object after the Scala typer runs, so a
 * source-level reference in a Scala test file does not compile. The plugin
 * also transforms Scala test sources (it is applied to {@code lib}'s Test
 * configuration), which occasionally rewrites innocuous reflection calls.
 * Placing this helper in a Java file sidesteps the plugin entirely.
 *
 * <p>{@code DFApp} keeps mutable state (notably {@code designArgs}) on the
 * singleton and overwrites it on every {@code main} call. For test
 * independence each invocation saves the state before {@code main} and
 * restores it afterwards.
 */
public final class DesignArgsCLIHelper {

    private DesignArgsCLIHelper() {}

    private static final String MODULE_CLASS = "app.top_TestCLIFoo$";

    public static void invokeTopTestCLIFoo(String[] args) {
        try {
            Class<?> cls = Class.forName(MODULE_CLASS);
            Object instance = cls.getField("MODULE$").get(null);
            Field designArgsField = findField(instance.getClass(), "designArgs");
            designArgsField.setAccessible(true);
            Object savedDesignArgs = designArgsField.get(instance);
            try {
                Method method = cls.getMethod("main", String[].class);
                method.invoke(instance, (Object) args);
            } finally {
                designArgsField.set(instance, savedDesignArgs);
            }
        } catch (InvocationTargetException ite) {
            Throwable cause = ite.getCause();
            if (cause instanceof RuntimeException) {
                throw (RuntimeException) cause;
            }
            if (cause instanceof Error) {
                throw (Error) cause;
            }
            throw new RuntimeException(cause);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    private static Field findField(Class<?> cls, String name) {
        for (Class<?> c = cls; c != null; c = c.getSuperclass()) {
            for (Field f : c.getDeclaredFields()) {
                if (f.getName().equals(name) || f.getName().endsWith("$$" + name)) {
                    return f;
                }
            }
        }
        throw new RuntimeException("Field '" + name + "' not found on " + cls);
    }
}
