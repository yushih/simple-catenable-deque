open Printf
open Deque

/*
let render_suspend = s => {
  let CatDeque.Susp(_labda, repr) = s;
  let render_repr = r => {
    switch (r) {
    |CatDeque.Lazy_empty => <div>{text("Empty")}</div>
    |CatDeque.Lazy_tail_force =>  
    }
  };
  <div> 
    {text("||")}
  </div>
}
*/

let text = ReasonReact.string;

let render_deque = q => {
  let rec render_cat : 'a.(('a=>ReasonReact.reactElement, CatDeque.cat('a))=>ReasonReact.reactElement) = (render_elem, q) => {
    let render_base = q => { // box already rendered by caller
      let elems = Array.map(render_elem, q);
      <div className="basic-box">
         (if (Array.length(elems)==0) {
           <span dangerouslySetInnerHTML={{ "__html": "&empty;" }} />
         } else {
           ReasonReact.array(elems)
         })
      </div>
    };
    

    switch (q) {
    |CatDeque.Shallow(arr) =>
      <div className="render-box content-member">
        <div className="title-box shallow">
          {text("Shallow")}
        </div>
        <div className="content-box">
          {render_base(arr)}
        </div>
      </div>
    |CatDeque.Deep(l, m, r) =>
      let CatDeque.Susp(lambda, repr) = m;
      let v = lambda();
      <div className="render-box content-member">
        <div className="title-box deep">
          {text("Deep")}
        </div>
        <div className="content-box">
          {render_base(l)}
          {render_cat(render_base, v)}
          {render_base(r)}
        </div>
      </div>
    }//switch
  };

  let render_number = i => <div className="int-box content-member">{text(string_of_int(i))}</div>;
  render_cat(render_number, q)
}

type state = {
  current_deq: CatDeque.cat(int),
  next_cons: string,
  next_snoc: string,
  expr: string,
}

type action = 
  | NextConsChange(string)
  | Cons
  | NextSnocChange(string)
  | Snoc
  | ConcatSelf
  | Tail
;

let component = ReasonReact.reducerComponent("Main")

let make = _children => {
    ...component,

    initialState: ()=> 
      /*
         let deq = ref(CatDeque.empty)

         for (i in 1 to 10) {
         deq := CatDeque.cons(i, deq^)
         }

         deq := CatDeque.concat(deq^, deq^)
       */
    {
      current_deq: CatDeque.empty,
      next_cons: "0",
      next_snoc: "0",
      expr: "empty",
    },

    reducer: (action, state) => 
      switch (action) {
      |NextConsChange(value) => ReasonReact.Update({...state, next_cons: value})
      |Cons => {
        let next_cons = int_of_string(state.next_cons);
        ReasonReact.Update(
          {...state, 
           current_deq: CatDeque.cons(next_cons, state.current_deq),
           next_cons: string_of_int(next_cons+1),
           expr: sprintf("cons(%d, %s)", next_cons, state.expr)
          })
        }
      |NextSnocChange(value) => ReasonReact.Update({...state, next_snoc: value})
      |Snoc => {
        let next_snoc = int_of_string(state.next_snoc);
        ReasonReact.Update(
          {...state, 
           current_deq: CatDeque.snoc(state.current_deq, next_snoc),
           next_snoc: string_of_int(next_snoc-1),
           expr: sprintf("snoc(%s, %d)", state.expr, next_snoc)
          })
        }
       |ConcatSelf => 
         ReasonReact.Update({
           ...state, 
           current_deq: CatDeque.concat(state.current_deq, state.current_deq),
           expr: sprintf("concat(%s, %s)", state.expr, state.expr)
         })
       |Tail => 
         ReasonReact.Update({
           ...state, 
           current_deq: CatDeque.tail(state.current_deq),
           expr: sprintf("tail(%s)", state.expr),
         })
       },//switch (action)

    render: self =>
      <div>
        <input 
          type_="text" 
          value=(self.state.next_cons)
          onChange=(e => self.send(NextConsChange(ReactEvent.Form.target(e)##value)))></input>
        <button onClick=(_e => self.send(Cons))>{text("cons")}</button>
        <br />

        <input 
          type_="text" 
          value=(self.state.next_snoc)
          onChange=(e => self.send(NextSnocChange(ReactEvent.Form.target(e)##value)))></input>
        <button onClick=(_e => self.send(Snoc))>{text("snoc")}</button>
        <br/>

        <button onClick=(_e => self.send(ConcatSelf))>(text("concat with self"))</button>

        <button disabled=(CatDeque.isEmpty(self.state.current_deq)) onClick=(_e => self.send(Tail))>(text("tail"))</button>
        <div>(text(self.state.expr))</div>
        <div className="root-box">
          {render_deque(self.state.current_deq)}
        </div>
      </div>
};
