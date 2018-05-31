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

	static <T> void covariantF(Container<? extends T> container) {
		T t = container.get();
//		container.add(t);
	}

	static <T> void contravariantF(Container<? super T> container) {
//		T t = container.get();
		container.add((T) null);
	}

	static <T> void invariantF(Container<T> container) {
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
		C.<Super>covariantF(new Container<Super>());
		C.<Super>covariantF(new Container<Base>());
		C.<Super>covariantF(new Container<Sub>());

//		C.<Base>covariantF(new Container<Super>());
		C.<Base>covariantF(new Container<Base>());
		C.<Base>covariantF(new Container<Sub>());

//		C.<Sub>covariantF(new Container<Super>());
//		C.<Sub>covariantF(new Container<Base>());
		C.<Sub>covariantF(new Container<Sub>());
	}

	@Test public void contravariantContainer() {
		C.<Super>contravariantF(new Container<Super>());
//		C.<Super>contravariantF(new Container<Base>());
//		C.<Super>contravariantF(new Container<Sub>());

		C.<Base>contravariantF(new Container<Super>());
		C.<Base>contravariantF(new Container<Base>());
//		C.<Base>contravariantF(new Container<Sub>());

		C.<Sub>contravariantF(new Container<Super>());
		C.<Sub>contravariantF(new Container<Base>());
		C.<Sub>contravariantF(new Container<Sub>());
	}

	@Test public void invariantContainer() {
		C.<Super>invariantF(new Container<Super>());
//		C.<Super>invariantF(new Container<Base>());
//		C.<Super>invariantF(new Container<Sub>());

//		C.<Base>invariantF(new Container<Super>());
		C.<Base>invariantF(new Container<Base>());
//		C.<Base>invariantF(new Container<Sub>());

//		C.<Sub>invariantF(new Container<Super>());
//		C.<Sub>invariantF(new Container<Base>());
		C.<Sub>invariantF(new Container<Sub>());
	}
}
