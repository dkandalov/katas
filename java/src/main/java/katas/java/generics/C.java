package katas.java.generics;

import org.junit.Test;

@SuppressWarnings({"unused", "RedundantTypeArguments", "Convert2Diamond", "RedundantCast", "UnusedAssignment", "SillyAssignment", "ConstantConditions"})
public class C {

	static class Super {}
	static class Base extends Super {}
	static class Sub extends Base {}

	static class Container<T> {
		void add(T t) {}
		T get() { return null; }
	}

	static <T> void f(T t) {}

	static <T> void covF(Container<? extends T> container) {
		T t = container.get();
//		container.add(t);
	}

	static <T> void contF(Container<? super T> container) {
//		T t = container.get();
		container.add((T) null);
	}

	static <T> void invF(Container<T> container) {
		T t = container.get();
		container.add(t);
	}

	public void subTypes() {
		C.<Super>f((Super) null);
		C.<Super>f((Base) null);
		C.<Super>f((Sub) null);

//		C.<Base>f((Super) null);
		C.<Base>f((Base) null);
		C.<Base>f((Sub) null);

//		C.<Sub>f((Super) null);
//		C.<Sub>f((Base) null);
		C.<Sub>f((Sub) null);
	}

	@Test public void covariantContainer() {
		C.<Super>covF(new Container<Super>());
		C.<Super>covF(new Container<Base>());
		C.<Super>covF(new Container<Sub>());

//		C.<Base>covF(new Container<Super>());
		C.<Base>covF(new Container<Base>());
		C.<Base>covF(new Container<Sub>());

//		C.<Sub>covF(new Container<Super>());
//		C.<Sub>covF(new Container<Base>());
		C.<Sub>covF(new Container<Sub>());
	}

	@Test public void contravariantContainer() {
		C.<Super>contF(new Container<Super>());
//		C.<Super>contF(new Container<Base>());
//		C.<Super>contF(new Container<Sub>());

		C.<Base>contF(new Container<Super>());
		C.<Base>contF(new Container<Base>());
//		C.<Base>contF(new Container<Sub>());

		C.<Sub>contF(new Container<Super>());
		C.<Sub>contF(new Container<Base>());
		C.<Sub>contF(new Container<Sub>());
	}

	@Test public void invariantContainer() {
		C.<Super>invF(new Container<Super>());
//		C.<Super>invF(new Container<Base>());
//		C.<Super>invF(new Container<Sub>());

//		C.<Base>invF(new Container<Super>());
		C.<Base>invF(new Container<Base>());
//		C.<Base>invF(new Container<Sub>());

//		C.<Sub>invF(new Container<Super>());
//		C.<Sub>invF(new Container<Base>());
		C.<Sub>invF(new Container<Sub>());
	}
}
