package katas.java.future;

import org.junit.Test;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static java.util.concurrent.CompletableFuture.supplyAsync;
import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.*;
import static katas.java.util.Misc.println;
import static katas.java.util.Misc.sleep;

// see http://www.nurkiewicz.com/2013/05/java-8-definitive-guide-to.html
public class CompletableFutureTest {
    @Test public void a() throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future = new CompletableFuture<>();

        new Thread(() -> {
            println("Sleeping");
            sleep(100);
            println("Woke up");
            future.complete(42);
        }).start();

        assertThat(future.get(), equalTo(42));
    }


    @Test public void b() throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future = supplyAsync(() -> {
            println("Sleeping");
            sleep(100);
            println("Woke up");
            return 42;
        });

        assertThat(future.get(), equalTo(42));
    }

    @Test public void c() throws ExecutionException, InterruptedException {
        AtomicBoolean flag = new AtomicBoolean();
        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
            println("Sleeping");
            sleep(100);
            println("Woke up");
            flag.set(true);
        });

        assertThat(future.get(), equalTo(null));
        assertTrue(flag.get());
    }

    @Test public void d() throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future1 = supplyAsync(() -> {
            println("Sleeping on " + Thread.currentThread().getName());
            sleep(100);
            println("Woke up");
            return 1;
        });
        CompletableFuture<Integer> future2 = future1.thenApply((n) -> {
            println("Sleeping on " + Thread.currentThread().getName());
            sleep(10);
            println("Woke up");
            return n + 2;
        });
        CompletableFuture<Integer> future3 = future2.thenApplyAsync((n) -> {
            println("Sleeping on " + Thread.currentThread().getName());
            sleep(10);
            println("Woke up");
            return n + 3;
        });

        assertThat(future3.get(), equalTo(6));
        assertThat(future2.get(), equalTo(3));
        assertThat(future1.get(), equalTo(1));
    }

    @Test public void e() {
        AtomicBoolean wasAccepted = new AtomicBoolean();
        AtomicBoolean wasRun = new AtomicBoolean();

        CompletableFuture<Integer> future = supplyAsync(() -> {
            println("Sleeping");
            sleep(100);
            println("Woke up");
            return 1;
        });

        future.thenAccept(n -> wasAccepted.set(true));
        future.thenRun(() -> wasRun.set(true));
        future.join();

        assertTrue(wasAccepted.get());
        assertTrue(wasRun.get());
    }

    @Test public void f() throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future = new CompletableFuture<>();
        CompletableFuture<Integer> future1 = future.exceptionally(e -> -42);
        CompletableFuture<Integer> future2 = future.handle((n, e) -> {
            if (n != null) {
                return n + 1;
            } else {
                return -12;
            }
        });

        new Thread(() -> {
            sleep(100);
            future.completeExceptionally(new Exception());
        }).start();

        assertThat(future1.get(), equalTo(-42));
        assertThat(future2.get(), equalTo(-12));
    }

    @Test public void g() throws ExecutionException, InterruptedException {
        Function<Integer, CompletionStage<Integer>> increment = (n) -> supplyAsync(() -> n + 1);
        CompletableFuture<Integer> future = supplyAsync(() -> 1).thenCompose(increment).thenCompose(increment);

        assertThat(future.get(), equalTo(3));
    }

    @Test public void h() throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future1 = supplyAsync(() -> 1);
        CompletableFuture<Integer> future2 = supplyAsync(() -> 2);
        CompletableFuture<Integer> future3 = future1.thenCombine(future2, (a, b) -> a + b);

        assertThat(future3.get(), equalTo(3));
    }

    @Test public void j() throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future1 = supplyAsync(() -> 1);
        CompletableFuture<Integer> future2 = supplyAsync(() -> 2);

        AtomicInteger value = new AtomicInteger();
        AtomicBoolean afterBoth = new AtomicBoolean();
        future1.thenAcceptBoth(future2, (a, b) -> value.set(a + b)).join();
        future1.runAfterBoth(future2, () -> afterBoth.set(true));

        assertThat(value.get(), equalTo(3));
        assertTrue(afterBoth.get());
    }
    
    @Test public void k() throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> future1 = supplyAsync(() -> 1);
        CompletableFuture<Integer> future2 = supplyAsync(() -> 2);

        AtomicInteger value = new AtomicInteger();
        AtomicBoolean afterEither = new AtomicBoolean();

        CompletableFuture<Integer> future3 = future1.applyToEither(future2, (n) -> n + 1);
        CompletableFuture<Void> future4 = future1.acceptEither(future2, value::set);
        CompletableFuture<Void> future5 = future1.runAfterEither(future2, () -> afterEither.set(true));

        future3.join();
        future4.join();
        future5.join();

        assertThat(future3.get(), anyOf(equalTo(2), equalTo(3)));
        assertThat(value.get(), anyOf(equalTo(1), equalTo(2)));
        assertTrue(afterEither.get());
    }
}
