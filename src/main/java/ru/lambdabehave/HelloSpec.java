package ru.lambdabehave;

import com.insightfullogic.lambdabehave.JunitSuiteRunner;
import com.insightfullogic.lambdabehave.Suite;
import org.junit.runner.RunWith;
import ru.skiplist.SkipList0;

@RunWith(JunitSuiteRunner.class)
@SuppressWarnings("ClassInitializerMayBeStatic")
public class HelloSpec {{
    Suite.describe("skip list", it -> {
        it.should("have size", expect -> {
            expect.that(new SkipList0().isEmpty()).is(true);
        });
    });
}}
