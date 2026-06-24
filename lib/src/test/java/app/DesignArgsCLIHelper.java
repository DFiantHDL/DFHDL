package app;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Reflection helper for invoking the DFHDL plugin-generated entry point of
 * {@code TestCLIFoo} from the Scala test suite.
 *
 * <p>The DFHDL plugin injects a {@code main} into the design's companion object
 * after the Scala typer runs, so a source-level reference in a Scala test file
 * does not compile. The plugin also transforms Scala test sources (it is
 * applied to {@code lib}'s Test configuration), which occasionally rewrites
 * innocuous reflection calls. Placing this helper in a Java file sidesteps the
 * plugin entirely.
 *
 * <p>Each {@code main} call now instantiates a fresh {@code DFApp} internally,
 * so its mutable state (e.g. {@code designArgs}) is per-invocation and no
 * save/restore of singleton state is required for test independence.
 */
public final class DesignArgsCLIHelper {

    private DesignArgsCLIHelper() {}

    private static final String MODULE_CLASS = "app.TestCLIFoo$";

    public static void invokeTopTestCLIFoo(String[] args) {
        try {
            Class<?> cls = Class.forName(MODULE_CLASS);
            Object instance = cls.getField("MODULE$").get(null);
            Method method = cls.getMethod("main", String[].class);
            method.invoke(instance, (Object) args);
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
}
