package name.xps;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;

public class Json {

	public enum Type {
		STRING, NUMBER, BOOLEAN, OBJECT, ARRAY, BEGIN, NULL, STRUCT
	}

	Type type;
	Object value;

	public Json(String s) {
		this.type = Type.STRING;
		this.value = s;
	}

	public Json() {
		this.type = Type.NULL;
		this.value = null;
	}

	public Json(LinkedHashMap<String, Json> j) {
		this.type = Type.OBJECT;
		this.value = j;
	}

	public Json(LinkedList<Json> j) {
		this.type = Type.ARRAY;
		this.value = j;
	}

	public Json(boolean b) {
		this.type = Type.BOOLEAN;
		this.value = Boolean.valueOf(b);
	}

	public Json(Number n) {
		this.type = Type.NUMBER;
		this.value = n;
	}
	
	public String toString(){
		StringBuilder sb=new StringBuilder();
		switch(this.type){
		case ARRAY:
			sb.append('[');
			boolean first=true;
			Iterator<Json> it=getArray();
			while(it.hasNext()){
				if(!first)sb.append(',');
				else first=false;
				sb.append(it.next().toString());
			}
			sb.append(']');
			return sb.toString();
		case OBJECT:
			sb.append('{');
			first=true;
			Map<String,Json> m=getObject();
			for(String key:m.keySet()){
				if(!first)sb.append(',');
				else first=false;
				sb.append('"'+key+'"'+":");
				sb.append(m.get(key).toString());
			}
			sb.append('}');
			return sb.toString();
		case STRING:
			return '"'+(String) value+'"';
		case NUMBER:
			return ((Number)value).toString();
		case NULL:
			return "null";
		case BOOLEAN:
			return ((Boolean)value).toString();
		}
		return null;
	}
	
	@SuppressWarnings("unchecked")
	public Iterator<Json> getArray(){
		if(this.type==Type.ARRAY){
			return ((LinkedList<Json>)value).iterator();
		}
		throw new UnsupportedOperationException("");
	}
	
	@SuppressWarnings("unchecked")
	public Map<String,Json> getObject(){
		if(this.type==Type.OBJECT){
			return (Map<String,Json>)value;
		}
		throw new UnsupportedOperationException("");
	}

	private static class BackInputStream extends InputStream {
		private InputStream in;
		private boolean back;
		private int backByte;

		public BackInputStream(InputStream in) {
			super();
			this.in = in;
			this.backByte = -1;
			this.back = false;
		}

		@Override
		public int read() throws IOException {
			if (back) {
				back = false;
			} else {
				backByte = in.read();
			}
			return backByte;
		}

		public void back() {
			assert back = false;
			back = true;
		}

	}

	static public Json parse(InputStream in) throws IOException {
		return parse(new BackInputStream(in), Type.BEGIN, 0);
	}

	static final byte[] NULL_ARRAY = { 'u', 'l', 'l' };
	static final byte[] TRUE_ARRAY = { 'r', 'u', 'e' };
	static final byte[] FALSE_ARRAY = { 'a', 'l', 's', 'e' };
	static final byte[] HEX_ARRAY = { '0', '1', '2', '3', '4', '5', '6', '7',
			'8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e',
			'f' };

	static public Json parse(BackInputStream in, Type upType, int layer)
			throws IOException {
		int c;
		Json j;
		while ((c = in.read()) != -1) {
			switch (c) {
			case 0x20:
			case 0x09:
			case 0x0A:
			case 0x0D:
				break;
			case 0x22:// "
				StringBuilder sb = new StringBuilder();
				while ((c = in.read()) != -1) {
					if (c == 0x22) {
						return new Json(sb.toString());
					} else if (c == 0x5c) {
						c = in.read();
						switch (c) {
						case 0x22: 
							sb.append('"');
							break;
						case 0x5C:
							sb.append('\\');
							break;
						case 0x2F:
							sb.append('/');
							break;
						case 0x62:
							sb.append((char)0x08);
							break;
						case 0x66:
							sb.append('\f');
							break;
						case 0x6E:
							sb.append('\n');
							break;
						case 0x72:
							sb.append('\r');
							break;
						case 0x74:
							sb.append('\t');
							break;
						case 0x75:
							byte[] hex=new byte[4];
							for(int i=0;i<4;i++){
								if((c=in.read())==-1)throw new IllegalArgumentException("damn end while in a string ");
								hex[i]=(byte) c;
							}
							try{
								sb.append((char)Integer.parseInt(new String(hex),16));
							}catch(NumberFormatException e){
								throw new IllegalArgumentException("error u escape in string");
							}
							
							break;
						default:
							throw new IllegalArgumentException("error escape in string");
						}
					} else {
						sb.append((char) c);
					}
				}

				break;
			case 0x5b:// [
				LinkedList<Json> jArray = new LinkedList<Json>();
				while ((j = parse(in, Type.ARRAY, layer + 1)) != null) {
					jArray.add(j);
					int st = nextStruct(in);
					if (st == 0x5d)
						break;
					if (st != 0x2c)
						throw new IllegalArgumentException(", or ] expected.");
				}
				return new Json(jArray);
			case 0x5d:// ]
				if (upType == Type.ARRAY) {
					return null;
				} else {
					throw new IllegalArgumentException("unalowable ] occered");
				}
			case 0x7b:// {
				LinkedHashMap<String, Json> jObject = new LinkedHashMap<String, Json>();
				String name;
				while ((j = parse(in, Type.OBJECT, layer + 1)) != null) {
					if (j.type == Type.STRING) {
						name = (String) j.value;
					} else {
						throw new IllegalArgumentException(
								"Object key must a String.");
					}
					if (nextStruct(in) != 0x3a)
						throw new IllegalArgumentException(": expected");
					j = parse(in, Type.OBJECT, layer + 1);
					if (j == null)
						throw new IllegalArgumentException(
								"Object value expected");
					jObject.put(name, j);
					int st = nextStruct(in);
					if (st == 0x7d)
						break;
					if (st != 0x2c)
						throw new IllegalArgumentException(", or } expected.");
				}
				return new Json(jObject);
			case 0x7d:// }
				if (upType == Type.OBJECT) {
					return null;
				} else {
					throw new IllegalArgumentException("unalowable } occered");
				}
			case 0x3a:// :
			case 0x2c:// ,
				throw new IllegalArgumentException("unexpected : or ,");

			case 'n':
				byte[] n = new byte[3];
				in.read(n);
				if (Arrays.equals(n, NULL_ARRAY))
					return new Json();
				throw new IllegalArgumentException("n ull expected.");
			case 'f':
				n = new byte[4];
				in.read(n);
				if (Arrays.equals(n,FALSE_ARRAY))
					return new Json(false);
				throw new IllegalArgumentException("f alse expected.");
			case 't':
				n = new byte[3];
				in.read(n);
				if (Arrays.equals(n,TRUE_ARRAY))
					return new Json(true);
				throw new IllegalArgumentException("t rue expected.");
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '-':
				boolean isNegative = false;
				boolean pointSeen = false;
				byte[] dec = new byte[10];
				short decLength = 0;
				int integer = 0;
				boolean valid = false;
				if (c == '-') {
					isNegative = true;
					c = in.read();
				}
				do {
					switch (c) {
					case '0':

					case '1':
					case '2':
					case '3':
					case '4':
					case '5':
					case '6':
					case '7':
					case '8':
					case '9':
						if (pointSeen) {
							if (decLength > 9)
								break; // Max decimal length is 10
							dec[decLength] = (byte) c;
							decLength++;
							valid = true;
						} else {
							if (integer == 0 && valid)
								throw new IllegalArgumentException(
										"leading zero is not allowed");
							integer = integer * 10 + (c - 0x30);
							valid = true;
						}
						break;
					case '.':
						if (pointSeen) {
							throw new IllegalArgumentException(". already have");
						} else if (valid) {
							pointSeen = true;
							valid = false;
						} else
							throw new IllegalArgumentException(
									". should not happen");
						break;
					case 'E':
					case 'e':
					case '-':
					case '+':
						throw new IllegalArgumentException(
								"not support E number yet");
					default:
						if (valid) {
							in.back();
							if (pointSeen) {
								double f = 0;
								for (int i = decLength; i > 0; i--) {
									f = (f + dec[i - 1]) * 0.1;
								}
								f = f + integer;
								return new Json(f);
							} else {
								if (isNegative)
									integer = -integer;
								return new Json(integer);
							}
						} else {
							throw new IllegalArgumentException(
									"error while parse number");
						}

					}
				} while ((c = in.read()) != -1);
			default:
			}
		}
		return null;
	}

	static private int nextStruct(InputStream in) throws IOException {
		int c;
		while ((c = in.read()) != -1) {
			switch (c) {
			case 0x20:
			case 0x09:
			case 0x0A:
			case 0x0D:
				break;
			case 0x5b:// [
			case 0x5d:// ]
			case 0x7b:// {
			case 0x7d:// }
			case 0x3a:// :
			case 0x2c:// ,
				return c;
			default:
				throw new IllegalArgumentException("Expect token but not.");
			}
		}
		throw new IllegalArgumentException("Expect token but end.");

	}

	public static void main(String[] args) throws IOException {
		Json j=parse(new FileInputStream("d:\\desktop\\json.txt"));
		System.out.println(j);
	}

}
