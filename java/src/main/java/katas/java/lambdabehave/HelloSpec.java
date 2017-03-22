package katas.java.lambdabehave;

import com.insightfullogic.lambdabehave.JunitSuiteRunner;
import com.insightfullogic.lambdabehave.Suite;
import org.junit.runner.RunWith;
import katas.java.skiplist.SkipList0;

@RunWith(JunitSuiteRunner.class)
@SuppressWarnings("ClassInitializerMayBeStatic")
public class HelloSpec {{
    Suite.describe("skip list", it -> {
        it.should("have size", expect -> {
            expect.that(new SkipList0().isEmpty()).is(true);
        });
    });
}}
