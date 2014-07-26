package ru.lambdabehave;

import com.insightfullogic.lambdabehave.JunitSuiteRunner;
import com.insightfullogic.lambdabehave.Suite;
import org.junit.runner.RunWith;

@RunWith(JunitSuiteRunner.class)
@SuppressWarnings("ClassInitializerMayBeStatic")
public class HelloSpec {{
    Suite.describe("skip list", it -> {
        it.should("have size", expect -> {
            expect.that(new SkipList().isEmpty()).is(true);
        });
    });
}

    private static class SkipList {
        public boolean isEmpty() {
            return false;
        }
    }
}
